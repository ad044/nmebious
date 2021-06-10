(in-package #:nmebious)

;; Random utils
(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun hex (bytes)
  (byte-array-to-hex-string bytes))

(defmacro cassoc (item alist)
  `(cdr (assoc ,item ,alist)))

(defun numeric-string-p (string)
  (ignore-errors (parse-integer string)))

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
          (send-error "Bad request."))
        (catch-error (e)
          (send-error (message e))))
     (handler-bind ((request-error #'catch-error)
                    (error #'fail))
       ,@body)))

(defmacro with-allowed-check ((&key ip-hash checksum text-data content-type post-get-count)
                              &body body)
  `(cond ((banned-p ,ip-hash)
          (throw-request-error "You are banned."))
         ((and ,content-type
               (not (mime-type-accepted-p ,content-type)))
          (throw-request-error (format nil "Content type not accepted: ~A." ,content-type)))
         ((and ,*allow-duplicates-after*
               (or (and ,checksum
                        (file-duplicate-p ,checksum ,ip-hash ,*allow-duplicates-after*))
                   (and ,text-data
                        (text-duplicate-p ,text-data ,ip-hash ,*allow-duplicates-after*))))
          (throw-request-error "Duplicate post."))
         ((and ,post-get-count
               (> ,post-get-count ,*post-get-limit*))
          (throw-request-error "Tried to retrieve too many posts."))
         ((and ,text-data
               (> (length ,text-data) 255))
          (throw-request-error "Text too long."))
         (t (progn
              ,@body))))

(defun parse-get-count-from-string (n)
  (let* ((trimmed-n (string-trim '(#\Space #\Newline #\Backspace #\Tab
                                   #\Linefeed #\Page #\Return #\Rubout)
                                 n)))
    (cond ((string-equal "" trimmed-n) 5)
          ((numeric-string-p trimmed-n) (parse-integer n))
          (t (throw-request-error (format nil  "Post count must be a number, got: ~A." trimmed-n))))))

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
  (cond ((string-equal type "txt")
         'text-post)
        ((string-equal type "file")
         'file-post)
        ((string-equal type "all")
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

(defun sort-posts-by-date (a b)
  (timestamp< (cassoc :submission-date a)
              (cassoc :submission-date b)))

(defun format-image-link (filename)
  (when filename
    (format nil "~Astatic/~A" *web-url* filename)))
