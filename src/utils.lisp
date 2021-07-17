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

(defparameter *success* (encode-json-alist-to-string (pairlis '(status) '("Success"))))

(defun define-static-resource (uri path)
  (push (create-folder-dispatcher-and-handler
         uri path)
        *dispatch-table*))

;; Crypto
(defun hmac-sha256-bytes (secret text)
  (let ((hmac (make-hmac (ascii-string-to-byte-array secret) :sha256)))
    (update-hmac hmac (ascii-string-to-byte-array text))
    (hmac-digest hmac)))

(defun hmac-sha256 (secret text)
  (hex (hmac-sha256-bytes secret text)))

(defun hash-ip (ip)
  (hmac-sha256 *secret* ip))

;; Request handling

(defmacro redirect-back-to-board ()
  `(redirect (format nil "/boards/~A" (session-value :board))))

(defun redirect-to-404 ()
  (redirect "/not-found"))

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
          (formatted-text (format nil "~A" submitted-text))
          (checksum (hex (md5sum-string formatted-text))))
     (after-text-post-validity-check (:text-data formatted-text
                                      :checksum checksum
                                      :board board
                                      :ip-hash ip-hash)
       (let* ((post-id (caar (insert-post formatted-text checksum "text" board ip-hash))))
         (when *socket-server-enabled-p*
           (broadcast :type 'text
                      :post-id post-id
                      :data formatted-text
                      :board board))
         ,@body))))

(defmacro after-submit-file (&body body)
  `(bind (((src filename content-type) (or (cassoc "file"
                                                   (post-parameters*)
                                                   :test #'string=)
                                           (throw-request-error "No file data found.")))
           (board (or (cassoc "board"
                              (post-parameters*)
                              :test #'string=)
                      (session-value :board)
                      (throw-request-error "No board data found.")))
           (ip-hash (hash-ip (real-remote-addr)))
           (checksum (hex (md5sum-file src))))
     (after-file-post-validity-check (:content-type content-type
                                      :checksum checksum
                                      :board board
                                      :ip-hash ip-hash)
       (let* ((filename (gen-filename))
              (type (mime-file-type content-type))
              (full-filename (concatenate 'string filename "." type))
              (dest (make-pathname :directory (pathname-directory *uploads-dir*)
                                   :name filename
                                   :type type)))
         (format-and-save-file src dest)
         (let* ((post-id (caar (insert-post full-filename checksum "file" board ip-hash))))
           (when *socket-server-enabled-p*
             (broadcast :type 'file
                        :post-id post-id
                        :data full-filename
                        :board board))
           ,@body)))))

(defmacro throw-request-error (msg)
  `(error 'request-error :message ,msg))

(defmacro with-fail-handler ((name &key type) &body body)
  `(labels
       ((send-error (e)
          (setf (return-code*) +http-bad-request+)
          (format *error-output* "Error: ~A~%" e)
          (return-from ,name
            (cond ((eql ,type 'api)
                   (encode-json-alist-to-string (pairlis '(message status)
                                                         (list e "Error"))))
                  ((eql ,type 'web-view-submission)
                   (progn
                     (setf (session-value :flash-message) e)
                     (redirect-back-to-board)))
                  ((eql ,type 'web-view)
                   (render-404)))))

        (fail (e)
          (format *error-output* "Error: ~A~%" e)
          (send-error "Bad request."))
        (catch-error (e)
          (send-error (message e))))
     (handler-bind ((request-error #'catch-error)
                    (error #'fail))
       ,@body)))

(defmacro after-get-request-validity-check ((&key count type board) &body body)
  `(progn
     (cond ((not (or (string= ,type "text")
                     (string= ,type "file")
                     (null ,type)))
            (throw-request-error "Incorrect type."))
           ((and ,board
                 (not (board-exists-p ,board)))
            (throw-request-error "Board does not exist."))
           ((and ,count
                 (> count *post-get-limit*))
            (throw-request-error "Tried to retrieve too many posts.")))
     ,@body))

(defmacro after-text-post-validity-check ((&key text-data checksum board ip-hash) &body body)
  `(progn
     (cond ((> (length ,text-data) 255)
            (throw-request-error "Text too long."))
           ((eql (length ,text-data) 0)
            (throw-request-error "The submitted text can't be empty."))
           ((not (board-exists-p ,board))
            (throw-request-error "Board does not exist."))
           ((and ,*allow-duplicates-after*
                 (post-duplicate-p ,checksum ,ip-hash ,*allow-duplicates-after* ,board))
            (throw-request-error "Duplicate post.")))
     ,@body))

(defmacro after-file-post-validity-check ((&key content-type checksum board ip-hash) &body body)
  `(progn
     (cond ((not (mime-type-accepted-p ,content-type))
            (throw-request-error (format nil "Content type not accepted: ~A." ,content-type)))
           ((not (board-exists-p ,board))
            (throw-request-error "Board does not exist."))
           ((and ,*allow-duplicates-after*
                 (post-duplicate-p ,checksum ,ip-hash ,*allow-duplicates-after* ,board))
            (throw-request-error "Duplicate post.")))
     ,@body))

(defun parse-board (board)
  (let* ((trimmed-board (string-trim '(#\Space #\Newline #\Backspace #\Tab
                                   #\Linefeed #\Page #\Return #\Rubout)
                                 board)))
    (cond ((string= "" trimmed-board) nil)
          (t trimmed-board))))

(defun board-exists-p (board)
  (member board *boards* :test #'string=))

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

(defun format-image (src dest)
  (list "convert"
        src
        "-resize" "30625@"
        "-colorspace" "gray"
        "-fill" "green"
        "-tint" "80"
        "-dither" "FloydSteinberg"
        "-colors" "3"
        "-brightness-contrast" (format nil "-~A" (random-in-range 20 50))
        dest))

(defun format-and-save-file (src dest)
  (let* ((dest-namestring (namestring dest))
         (src-namestring (namestring src)))
    (ensure-directories-exist dest)
    (run-program (format-image src-namestring dest-namestring))))

;; Text web view stuff
(defun get-font ()
  (let* ((fonts '("Times New Roman" "Times" "serif" "Arial"
                  "Helvetica" "sans-serif" "Georgia" "Courier New"
                  "Courier" "monospace")))
    (nth (random (length fonts)) fonts)))

(defun gen-color (hue)
  (let* ((sat (random-in-range 0 100))
         (lum (random-in-range 20 100)))
    (format nil "hsl(~A, ~A%, ~A%)" hue sat lum)))

(defun text-style ()
  (let* ((color (gen-color 120))
         (font-size (random-in-range 0.8 2.0))
         (left (random-in-range 0.1 40.0))
         (font-family (get-font)))
    (format nil
            "color: ~A; font-family: ~A; font-size: ~Aem; left: ~A%"
            color font-family font-size left)))

(defun corrupt (text)
  (let* ((corruptions (pairlis '(#\a #\e #\i #\o #\u #\y #\s)
                               '((#\á #\ã #\à #\@)
                                 (#\è #\ë #\ê)
                                 (#\ï #\î #\1)
                                 (#\ø #\ò #\ô)
                                 (#\ü #\ù)
                                 (#\ÿ)
                                 (#\$)))))
    (map 'string #'(lambda (char)
                     (let* ((corruptions-for-character (cassoc char corruptions)))
                       (if (and corruptions-for-character
                                (eql (random 2)
                                     1))
                           (nth (random (length corruptions-for-character)) corruptions-for-character)
                           char))) text)))

;; File web view stuff
(defun stylize-text-post (post)
  (acons :data (corrupt (cassoc :data post)) (acons :style (text-style) post)))

(defun file-style ()
  (let* ((z-index (random-in-range 1 10))
         (left (random-in-range 0.1 50.0))
         (opacity (random-in-range 0.5 1.0))
         (top (random-in-range 7.0 50.0)))
    (format nil
            "z-index: ~A; left: ~A%; opacity: ~A; top: ~A%" z-index left opacity top)))

(defun stylize-file-post (post)
  (acons :style (file-style) post))
