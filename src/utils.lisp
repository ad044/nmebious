(in-package #:nmebious)

;; Random utils
(defun random-in-range (start end)
  (+ start (random (- end start))))

(defun hex (bytes)
  (byte-array-to-hex-string bytes))

(defun cassoc (item alist &rest args &key &allow-other-keys)
  (cdr (apply #'assoc item alist args)))

(defun numeric-string-p (string)
  (ignore-errors (parse-integer string)))

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
  (loop for (key . value) in alist
        collect key))

(defun color-for-board (board)
  (cassoc :color (cassoc board *boards* :test #'string=)))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
               str))

(defun instance-has-backgrounds-p ()
  (> (length  (loop for (key . value)
                      in *boards*
                    when (cassoc :background value)
                      collect (cassoc :background value)))
     0))

(defun parse-user-preferences ()
  (if (cookie-in "mebious_user")
      (url-decode-params (cookie-in "mebious_user"))
      (progn
        (let* ((default-prefs (form-encoded-default-preferences)))
          (set-cookie "mebious_user"
                      :value default-prefs
                      :max-age 315360000
                      :path "/"
                      :secure t
                      :http-only t)
          (url-decode-params default-prefs)))))

(defun form-encoded-default-preferences ()
  (url-encode-params (reduce #'(lambda (acc pref)
                                 (acons (string-downcase (car pref))
                                        (if  (cassoc :default (cdr pref) :test #'string=)
                                             "on"
                                             "off")
                                        acc))
                             *web-user-preferences*
                             :initial-value '())))

;; Crypto
(defun hmac-sha256-bytes (secret text)
  (let ((hmac (make-hmac (ascii-string-to-byte-array secret) :sha256)))
    (update-hmac hmac (ascii-string-to-byte-array text))
    (hmac-digest hmac)))

(defun hmac-sha256 (secret text)
  (hex (hmac-sha256-bytes secret text)))

(defun hash-ip (ip)
  (hmac-sha256 *secret* ip))

(defun single-board-p ()
  (eql (length *boards*) 1))

;; Request handling
(defun api-success ()
  (encode-json-alist-to-string (pairlis '(status) '("Success"))))

(defun api-fail-with-message (msg)
  (setf (return-code*) +http-bad-request+)
  (encode-json-alist-to-string (pairlis '(message status)
                                        (list msg "Error"))))

(defmacro redirect-back-to-board ()
  `(if ,(single-board-p)
       (redirect "/")
       (redirect (format nil "/boards/~A" (session-value :board)))))

(define-condition request-error (error)
  ((message :initarg :message :reader message)))

(defmacro after-submit-text (&body body)
  `(let* ((post-params (post-parameters*))
          (ip-hash (hash-ip  (real-remote-addr)))
          (board (or (cassoc "board" post-params :test #'string=)
                     (session-value :board)
                     (throw-request-error "No board data found.")))
          (submitted-text (or (cassoc "text" post-params :test #'string=)
                              (throw-request-error "No text data found in body.")))
          (formatted-text (trim-whitespace (format nil "~A" submitted-text)))
          (checksum (hex (md5sum-string formatted-text))))
     (text-post-validity-check :text-data formatted-text
                               :checksum checksum
                               :board board
                               :ip-hash ip-hash)
     (let* ((post-id (caar (insert-post formatted-text checksum "text" board ip-hash))))
       (when *socket-server-enabled-p*
         (broadcast :type 'text
                    :post-id post-id
                    :data formatted-text
                    :board board))
       ,@body)))

(defmacro after-submit-file (&body body)
  `(bind ((post-params (post-parameters*))
          ((src filename content-type) (or (cassoc "file"
                                                   post-params
                                                   :test #'string=)
                                           (throw-request-error "No file data found.")))
          (board (or (cassoc "board"
                             post-params
                             :test #'string=)
                     (session-value :board)
                     (throw-request-error "No board data found.")))
          (ip-hash (hash-ip (real-remote-addr)))
          (checksum (hex (md5sum-file src))))
     (file-post-validity-check :content-type content-type
                               :checksum checksum
                               :board board
                               :ip-hash ip-hash)
     (let* ((filename (gen-filename))
            (type (mime-file-type content-type))
            (full-filename (concatenate 'string filename "." type))
            (dest (make-pathname :directory (append (pathname-directory *uploads-dir*) (list board))
                                 :name filename
                                 :type type)))
       (format-and-save-file src dest board)
       (let* ((post-id (caar (insert-post full-filename checksum "file" board ip-hash))))
         (when *socket-server-enabled-p*
           (broadcast :type 'file
                      :post-id post-id
                      :data full-filename
                      :board board))
         ,@body))))

(defmacro throw-request-error (msg)
  `(error 'request-error :message ,msg))

(defmacro with-fail-handler ((name &key type) &body body)
  `(labels
       ((send-error (e)
          (setf (return-code*) +http-bad-request+)
          (format *error-output* "Error: ~A~%" e)
          (return-from ,name
            (case ,type
              (api
               (api-fail-with-message e))
              (web-view-submission
               (progn
                 (setf (session-value :flash-message) e)
                 (redirect-back-to-board)))
              (web-view
               (render-404)))))

        (fail (e)
          (format *error-output* "Error: ~A~%" e)
          (send-error "Bad request."))
        (catch-error (e)
          (send-error (message e))))
     (handler-bind ((request-error #'catch-error)
                    (error #'fail))
       ,@body)))

(defun get-request-validity-check (&key count type board page offset)
  (cond ((and offset
              (< offset 0))
         (throw-request-error "Offset must be a positive number."))
        ((and page
              (< page 0))
         (throw-request-error "Page must be a positive number."))
        ((and count
              (< count 0))
         (throw-request-error "Count must be a positive number."))
        ((not (or (string= type "text")
                  (string= type "file")
                  (null type)))
         (throw-request-error "Incorrect type."))
        ((and board
              (not (board-exists-p board)))
         (throw-request-error "Board does not exist."))
        ((and count
              (> count *post-get-limit*))
         (throw-request-error "Tried to retrieve too many posts."))))

(defun text-post-validity-check (&key text-data checksum board ip-hash)
  (cond ((> (length text-data) 255)
         (throw-request-error "Text too long."))
        ((eql (length text-data) 0)
         (throw-request-error "Submitted text can't be empty."))
        ((not (board-exists-p board))
         (throw-request-error "Board does not exist."))
        ((and *allow-duplicates-after*
              (post-duplicate-p checksum ip-hash *allow-duplicates-after* board))
         (throw-request-error "Duplicate post."))))

(defun file-post-validity-check (&key content-type checksum board ip-hash)
  (cond ((not (mime-type-accepted-p content-type))
         (throw-request-error (format nil "Content type not accepted: ~A." content-type)))
        ((not (board-exists-p board))
         (throw-request-error "Board does not exist."))
        ((and *allow-duplicates-after*
              (post-duplicate-p checksum ip-hash *allow-duplicates-after* board))
         (throw-request-error "Duplicate post."))))

(defun parse-board (board)
  (let* ((trimmed-board (trim-whitespace board)))
    (cond ((string= "" trimmed-board) nil)
          (t trimmed-board))))

(defun board-exists-p (board)
  (member board (alist-keys *boards*) :test #'string=))

;; File saving/handling
(defun gen-filename ()
  (multiple-value-bind
        (second minute hour day month year)
      (get-decoded-time)
    (format nil "~{~A~#[~;-~:;~]~}"
            (list  year
                   month
                   day
                   hour
                   minute
                   second
                   (hex (random-data 10))))))

(defun mime-type-accepted-p (mime-type)
  (member mime-type *accepted-mime-types* :test #'string=))

(defun format-image (src dest board)
  (list "convert"
        src
        "-resize" "30625@"
        "-colorspace" "gray"
        "-fill" (color-for-board board)
        "-tint" "80"
        "-dither" "FloydSteinberg"
        "-colors" "3"
        "-brightness-contrast" (format nil "-~A" (random-in-range 20 50))
        dest))

(defun format-and-save-file (src dest board)
  (let* ((dest-namestring (namestring dest))
         (src-namestring (namestring src)))
    (ensure-directories-exist dest)
    (run-program (format-image src-namestring dest-namestring board))))
