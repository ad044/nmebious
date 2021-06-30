(in-package #:nmebious)

;; Random utils
(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun hex (bytes)
  (byte-array-to-hex-string bytes))

(defun cassoc (item alist &rest args &key &allow-other-keys)
  (cdr (apply #'assoc item alist args)))

(defun numeric-string-p (string)
  (ignore-errors (parse-integer string)))

(defun format-image-link (filename)
  (when filename
    (format nil "~Astatic/~A" *web-url* filename)))

(defun sort-posts-by-id (a b)
  (> (cassoc :post-id a)
     (cassoc :post-id b)))

(defparameter *success* (encode-json-alist-to-string (pairlis '(status) '("Success"))))

;; crypto
(defun hmac-sha256-bytes (secret text)
  (let ((hmac (make-hmac (ascii-string-to-byte-array secret) :sha256)))
    (update-hmac hmac (ascii-string-to-byte-array text))
    (hmac-digest hmac)))

(defun hmac-sha256 (secret text)
  (hex (hmac-sha256-bytes secret text)))

(defun hash-ip (ip)
  (hmac-sha256 *secret* ip))

;; Request handling
(define-condition request-error (error)
  ((message :initarg :message :reader message)))

(defmacro throw-request-error (msg)
  `(error 'request-error :message ,msg))

(defmacro with-fail-handler ((name) &body body)
  `(labels
       ((send-error (e)
          (setf (return-code*) +http-bad-request+)
          (format *error-output* "Error: ~A~%" e)
          (return-from ,name
            (encode-json-alist-to-string (pairlis '(message status)
                                                  (list e "Error")))))
        (fail (e)
          (format *error-output* "Error: ~A~%" e)
          (send-error "Bad request."))
        (catch-error (e)
          (send-error (message e))))
     (handler-bind ((request-error #'catch-error)
                    (error #'fail))
       ,@body)))

(defmacro with-allowed-check ((&key ip-hash checksum text-data content-type post-get-count board)
                              &body body)
  `(cond ((banned-p ,ip-hash)
          (throw-request-error "You are banned."))
         ((and ,content-type
               (not (mime-type-accepted-p ,content-type)))
          (throw-request-error (format nil "Content type not accepted: ~A." ,content-type)))
         ((and ,*allow-duplicates-after*
               (or (and ,checksum
                        (file-duplicate-p ,checksum ,ip-hash ,*allow-duplicates-after* ,board))
                   (and ,text-data
                        (text-duplicate-p ,text-data ,ip-hash ,*allow-duplicates-after* ,board))))
          (throw-request-error "Duplicate post."))
         ((and ,post-get-count
               (> ,post-get-count ,*post-get-limit*))
          (throw-request-error "Tried to retrieve too many posts."))
         ((and ,text-data
               (> (length ,text-data) 255))
          (throw-request-error "Text too long."))
         ((and ,text-data
               (eql (length ,text-data) 0))
          (throw-request-error "The submitted text can't be empty."))
         ((and ,board
               (not (board-exists-p ,board)))
          (throw-request-error "Board does not exist."))
         (t (progn
              ,@body))))

(defun parse-board-from-req (board)
  (let* ((trimmed-board (string-trim '(#\Space #\Newline #\Backspace #\Tab
                                   #\Linefeed #\Page #\Return #\Rubout)
                                 board)))
    (cond ((string= "" trimmed-board) nil)
          ((not (board-exists-p trimmed-board)) (throw-request-error "Board does not exist."))
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

(defun get-table-for-type (type)
  (cond ((string= type "txt")
         'text-post)
        ((string= type "file")
         'file-post)
        ((string= type "all")
         nil)
        (t (throw-request-error (format nil "Incorrect post type: ~A." type)))))

(defun format-image (src dest)
  (list "convert"
        src
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
