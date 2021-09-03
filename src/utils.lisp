(in-package #:nmebious)

(defun random-in-range (start end)
  (+ start (random (- end start))))

(defun hex (bytes)
  (byte-array-to-hex-string bytes))

(defun cassoc (item alist &rest args &key &allow-other-keys)
  (cdr (apply #'assoc item alist args)))

(defun sort-posts-by-id (a b)
  (> (cassoc :id a)
     (cassoc :id b)))

(defun without-last (l)
  (reverse (cdr (reverse l))))

(defun define-static-resource (uri path)
  (push (create-folder-dispatcher-and-handler
         uri path)
        *dispatch-table*))

(defun define-static-resource-file (uri path)
  (push (create-static-file-dispatcher-and-handler
         uri path)
        *dispatch-table*))

(defun alist-keys (alist)
  (mapcar #'car alist))

(defun color-for-board (board)
  (or (cassoc :color (cassoc board (get-config :boards) :test #'string=))
      "#00ff00"))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout)
               str))

(defun instance-has-backgrounds-p ()
  (remove-if-not (lambda (alist)
                   (cassoc :background (cdr alist)))
                 (get-config :boards)))

(defun parse-user-preferences ()
  (if (cookie-in "mebious-user")
      (url-decode-params (cookie-in "mebious-user"))
      (progn
        (let ((default-prefs (form-encoded-default-preferences)))
          (set-cookie "mebious-user"
                      :value default-prefs
                      :max-age 315360000
                      :path "/"
                      :secure t
                      :http-only t)
          (url-decode-params default-prefs)))))

(defun form-encoded-default-preferences ()
  (url-encode-params
   (reduce (lambda (acc pref)
             (acons (string-downcase (car pref))
                    (if  (cassoc :default (cdr pref)
				 :test #'string=)
                        "on"
                      "off")
                    acc))
           (get-config :web-user-preferences)
           :initial-value '())))

(defun single-board-p ()
  (eql (length (get-config :boards)) 1))

(defun word-filtered-p (word)
  (some (lambda (filter)
          (search filter word))
        (get-config :filtered-words)))

;; Crypto utils
(defparameter *argon2-kdf*
  (make-kdf :argon2d :block-count 15000))

(defun hash-ip (ip)
  (hex (md5sum-string ip)))

(defun hash-pass (pass salt)
  (hex (derive-key *argon2-kdf*
                   (ascii-string-to-byte-array pass)
                   salt
                   2
                   32)))

;; Request handling
(defun api-success ()
  (encode-json-alist-to-string
   (pairlis '(status) '("Success"))))

(defun api-fail-with-message (msg code)
  (setf (return-code*) code)
  (encode-json-alist-to-string
   (pairlis '(message status)
            (list msg "Error"))))

(defun redirect-back-to-board ()
  (if (single-board-p)
      (redirect "/")
      (redirect (format nil "/boards/~A" (session-value :board)))))

(define-condition request-error (error)
  ((message :initarg :message :reader message)))

(defmacro after-submit-text (&body body)
  `(let* ((post-params (post-parameters*))
          (ip-hash (hash-ip  (real-remote-addr)))
          (board (or (cassoc "board" post-params :test #'string=)
                     (session-value :board)
                     (throw-request-error "No board data found." :code 400)))
          (submitted-text (or (cassoc "text" post-params :test #'string=)
                              (throw-request-error "No text data found in body." :code 400)))
          (formatted-text (trim-whitespace (format nil "~A" submitted-text)))
          (html-escaped-text (string-escape-html formatted-text))
          (checksum (hex (md5sum-string html-escaped-text))))
     (text-post-validity-check :text-data html-escaped-text
                               :checksum checksum
                               :board board
                               :ip-hash ip-hash)
     (let ((post-id (insert-post html-escaped-text
				 checksum
				 "text"
				 board
				 ip-hash)))
       (when (get-config :socket-server-enabled-p)
         (broadcast-to-room :type 'text
			    :post-id post-id
			    :data html-escaped-text
			    :board board))
       ,@body)))

(defmacro after-submit-file (&body body)
  `(bind ((post-params (post-parameters*))
           ((src filename content-type) (or (cassoc "file"
                                                    post-params
                                                    :test #'string=)
                                            (throw-request-error "No file data found." :code 400)))
           (board (or (cassoc "board"
                              post-params
                              :test #'string=)
                      (session-value :board)
                      (throw-request-error "No board data found." :code 400)))
           (size-in-mb (/ (file-size-in-octets src)
                          1000000))
           (ip-hash (hash-ip (real-remote-addr)))
           (checksum (hex (md5sum-file src))))
     (file-post-validity-check :content-type content-type
                               :checksum checksum
                               :board board
                               :ip-hash ip-hash
                               :file-size size-in-mb)
     (let* ((filename (gen-filename))
            (type (mime-file-type content-type))
            (full-filename (concatenate 'string filename "." type))
            (dest (make-pathname :directory (append (pathname-directory (get-config :uploads-dir)) (list board))
                                 :name filename
                                 :type type)))
       (format-and-save-file src dest board)
       (let ((post-id (insert-post full-filename
				   checksum
				   "file"
				   board
				   ip-hash)))
         (when (get-config :socket-server-enabled-p)
           (broadcast-to-room :type 'file
			      :post-id post-id
			      :data full-filename
			      :board board))
         ,@body))))

(defun throw-request-error (msg &key code)
  (error 'request-error :message (pairlis '(:error-message :code)
                                          `(,msg ,code))))

(defmacro with-fail-handler ((name &key type) &body body)
  `(labels
       ((send-error (e code)
          (format *error-output* "Error: ~A~%" e)
          (return-from ,name
            (case ,type
              (api
               (api-fail-with-message e code))
              (web-view
               (render-error-page e code))
              (web-view-submission
               (progn
                 (setf (session-value :flash-message) e)
                 (redirect-back-to-board)))
              (admin-auth
               (progn
                 (setf (session-value :flash-message) e)
                 (redirect "/admin/auth"))))))
        (fail (e)
          (send-error "Bad request." 400))
        (catch-error (e)
          (let ((message (message e)))
            (send-error (cassoc :error-message message) (cassoc :code message)))))
     (handler-bind ((request-error #'catch-error)
                    (error #'fail))
       ,@body)))

(defmacro with-flash-message (&body body)
  `(let ((flash-message (session-value :flash-message)))
     (setf (session-value :flash-message) nil)
     (progn ,@body)))

(defun admin-auth-validity-check (username pass)
  (let ((lookup (car (admin-lookup username))))
    (cond ((or (not (cassoc :username lookup))
               (not (string= (hash-pass pass
                                        (hex-string-to-byte-array (cassoc :salt lookup)))
                             (cassoc :password lookup))))
           (throw-request-error "Invalid credentials." :code 401)))))

(defun get-request-validity-check (&key count type board page offset)
  (cond ((and offset
              (< offset 0))
         (throw-request-error "Offset must be a positive number." :code 422))
        ((and page
              (< page 0))
         (throw-request-error "Page must be a positive number." :code 422))
        ((and count
              (< count 0))
         (throw-request-error "Count must be a positive number." :code 422))
        ((not (or (string= type "text")
                  (string= type "file")
                  (null type)))
         (throw-request-error "Incorrect type." :code 422))
        ((and board
              (not (board-exists-p board)))
         (throw-request-error "Board does not exist." :code 404))
        ((and count
              (> count (get-config :post-get-limit)))
         (throw-request-error "Tried to retrieve too many posts." :code 422))))

(defun text-post-validity-check (&key text-data checksum board ip-hash)
  (cond ((> (length text-data) 255)
         (throw-request-error "Text too long." :code 413))
        ((eql (length text-data) 0)
         (throw-request-error "Submitted text can't be empty." :code 422))
        ((not (board-exists-p board))
         (throw-request-error "Board does not exist." :code 422))
        ((and (get-config :allow-duplicates-after)
              (post-duplicate-p checksum ip-hash (get-config :allow-duplicates-after) board))
         (throw-request-error "Duplicate post." :code 422))
        ((word-filtered-p text-data)
         (throw-request-error "Post cannot contain a filtered word." :code 422))
        ((banned-p ip-hash)
         (throw-request-error "You are banned." :code 403))))

(defun file-post-validity-check (&key content-type checksum board ip-hash file-size)
  (cond ((not (mime-type-accepted-p content-type))
         (throw-request-error (format nil "Content type not accepted: ~A." content-type) :code 422))
        ((not (board-exists-p board))
         (throw-request-error "Board does not exist." :code 422))
        ((and (get-config :allow-duplicates-after)
              (post-duplicate-p checksum ip-hash (get-config :allow-duplicates-after) board))
         (throw-request-error "Duplicate post." :code 422))
        ((banned-p ip-hash)
         (throw-request-error "You are banned." :code 403))
        ((> file-size (get-config :max-file-size))
         (throw-request-error (format nil "Submitted files can't be bigger than ~A MB." (get-config :max-file-size)) :code 413))))

(defun parse-board (board)
  (let ((trimmed-board (trim-whitespace board)))
    (cond ((string= "" trimmed-board) nil)
          (t trimmed-board))))

(defun board-exists-p (board)
  (member board (alist-keys (get-config :boards)) :test #'string=))

;; File saving/handling
(defun gen-filename ()
  (multiple-value-bind
        (second minute hour day month year)
      (get-decoded-time)
    (format nil "~{~A~#[~;-~:;~]~}"
            (list year
                  month
                  day
                  hour
                  minute
                  second
                  (hex (random-data 10))))))

(defun mime-type-accepted-p (mime-type)
  (member mime-type (get-config :accepted-mime-types) :test #'string=))

(defun format-image (src dest board)
  (list "convert"
        src
        "-resize" "30625@"
        "-colorspace" "gray"
        "-fill" (color-for-board board)
        "-tint" "80"
        "-dither" "FloydSteinberg"
        "-colors" "3"
        "-brightness-contrast" (format nil "~A" (random-in-range -25 0))
        dest))

(defun format-and-save-file (src dest board)
  (let* ((dest-namestring (namestring dest))
         (src-namestring (namestring src)))
    (ensure-directories-exist dest)
    (run-program (format-image src-namestring dest-namestring board))))

(defun string-escape-html (raw-string)
 (DJULA.FILTERS::force-escape raw-string))
