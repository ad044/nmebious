(defpackage #:nmebious-test
  (:use #:cl #:fiveam))

(in-package #:nmebious-test)

(def-suite :nmebious)

(in-suite :nmebious)

(def-fixture test-env ()
  (let* ((test-board (caar nmebious::*boards*)))
    (if (hunchentoot:started-p nmebious::*server*)
        (format t "The application can't be running while testing. Stop the server and retry.")
        (unwind-protect
             (progn
               (unless (hunchentoot:started-p nmebious::*server*)
                 (nmebious::start-hunchentoot))
               (postmodern:connect-toplevel "test"
                                            nmebious::*db-user*
                                            nmebious::*db-pass*
                                            "localhost")
               (nmebious::setup-db)
               (postmodern:query (:delete-from 'post))
               (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
                 (&body)))
          (progn
            (postmodern:disconnect-toplevel)
            (when (hunchentoot:started-p nmebious::*server*)
              (nmebious::stop-hunchentoot)))))))

(defun test-file (filename)
  (asdf:system-relative-pathname 'nmebious (format nil "t/~A" filename)))

(defun localhost (&rest path)
  (format nil
          "http://localhost:~A/~{~A~#[~;/~:;/~]~}"
          nmebious::*port*
          path))

(defmacro expect-success ()
  `(is (string= (nmebious::cassoc  :status
                                   (cl-json:decode-json-from-string body))
                    "Success"))
  `(is (eql code 200)))

(defmacro expect-error-with-message (message)
  `(let* ((json-body (cl-json:decode-json-from-string body))
                   (status (nmebious::cassoc :status json-body))
                   (message (nmebious::cassoc :message json-body)))
              (is (eql code 400))
              (is (string= "Error" status))
              (is (string= message ,message))))

(defmacro with-submit-text ((text board) &body body)
  `(multiple-value-bind (body code headers uri)
       (dexador:post  (localhost "api" "submit" "text")
                      :content (format nil "board=~A&text=~A" ,board ,text)
                      :headers '((:content-type . "application/x-www-form-urlencoded")))
     (declare (ignore headers uri))
     ,@body))

(defmacro with-submit-file ((path board) &body body)
  `(multiple-value-bind (body code headers uri)
       (dex:post (localhost "api" "submit" "file")
                 :content (pairlis '("file" "board")
                                   (list (test-file ,path) ,board)))
     (declare (ignore headers uri))
     ,@body))

(test submit-text
  (with-fixture test-env ()
    (postmodern:query (:delete-from 'post))

    ;; random string
    (with-submit-text ("firsttest" test-board)
      (expect-success))

    ;; integer
    (with-submit-text (3 test-board)
      (expect-success))

    ;; float
    (with-submit-text (10.24 test-board)
      (expect-success))

    ;; incorrect/nonexistant board
    (with-submit-text ("testphrase" "doesntexist")
      (expect-error-with-message "Board does not exist."))

    ;; duplicate checking works (if enabled)
    (when (and nmebious::*allow-duplicates-after*
               (> nmebious::*allow-duplicates-after* 0))
      (with-submit-text ("firsttest" test-board)
        (expect-error-with-message "Duplicate post.")))

    ;; text too long
    (with-submit-text ((make-string 256 :initial-element #\a) test-board)
      (expect-error-with-message "Text too long."))

    ;; incorrectly named key for text
    (multiple-value-bind (body code headers uri)
        (dexador:post  (localhost "api" "submit" "text")
                       :content (format nil "board=~A&incorrect=~A" test-board "test")
                       :headers '((:content-type . "application/x-www-form-urlencoded")))
      (declare (ignore headers uri))
      (expect-error-with-message "No text data found in body."))

    ;; incorrectly named key for board
    (multiple-value-bind (body code headers uri)
        (dexador:post  (localhost "api" "submit" "text")
                       :content (format nil "incorrect=~A&text=~A" test-board "test")
                       :headers '((:content-type . "application/x-www-form-urlencoded")))
      (declare (ignore headers uri))
      (expect-error-with-message "No board data found."))))

(test submit-file
  (with-fixture test-env ()
    (postmodern:query (:delete-from 'post))

    ;; jpg image (supported format)
    (when (nmebious::mime-type-accepted-p "image/jpeg")
      (with-submit-file ("test.jpg" test-board)
        (expect-success))

      ;; duplicate check (if enabled)
      (when (and nmebious::*allow-duplicates-after*
                 (> nmebious::*allow-duplicates-after* 0))
        (with-submit-file ("test.jpg" test-board)
          (expect-error-with-message "Duplicate post."))))

    ;; unsupported format
    (when (not (nmebious::mime-type-accepted-p "image/tiff"))
      (with-submit-file ("test.tiff" test-board)
        (expect-error-with-message "Content type not accepted: image/tiff." )))

    ;; incorrectly named key
    (multiple-value-bind (body code headers uri)
        (dex:post (localhost "api" "submit" "file")
                  :content (pairlis '("test" "board")
                                    (list (test-file "test.jpg") test-board)))
      (declare (ignore headers uri))
      (expect-error-with-message "No file data found."))))

(test get-posts
  (with-fixture test-env ()
    ;; insert test data
    (dotimes (i 3)
      (dexador:post  (localhost "api" "submit" "text")
                     :content (format nil "board=~A&text=~A" test-board i)
                     :headers '((:content-type . "application/x-www-form-urlencoded"))))
    (dex:post (localhost "api" "submit" "file")
              :content (pairlis '("file" "board")
                                (list (test-file "test.jpg") test-board)))

    ;; retrieving all types of posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts/"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        (is (eql (length json-body)
                 1))
        (is (eql (length (nmebious::cassoc :posts
                                           json-body))
                 4))))

    ;; retrieving only text posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts" "?type=text"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        (is (eql (length json-body)
                 1))
        (is (eql (length (nmebious::cassoc :posts
                                           json-body))
                 3))))

    ;; retriveing only 2 text posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts" "?type=text&count=2"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        (is (eql (length json-body)
                 1))
        (is (eql (length (nmebious::cassoc :posts
                                           json-body))
                 2))))

    ;; retrieving file posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts" "?type=file"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        (is (eql (length json-body)
                 1))
        (is (eql (length (nmebious::cassoc :posts
                                           json-body))
                 1))))

    ;; retrieving too many posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts" (format nil
                                                  "?type=text&count=~A"
                                                  (write-to-string (+ 1
                                                                      nmebious::*post-get-limit*)))))
      (let* ((json-body (cl-json:decode-json-from-string body))
             (message (nmebious::cassoc :message json-body)))
        (is (eql 400
                 code))
        (is (string= message "Tried to retrieve too many posts."))))

    ;; retrieving with negative offset
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts" (format nil
                                                  "?type=text&offset=-1")))
      (let* ((json-body (cl-json:decode-json-from-string body))
             (message (nmebious::cassoc :message json-body)))
        (is (eql 400
                 code))
        (is (string= message "Offset must be a positive number."))))

    ;; retrieving with negative count
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts" (format nil
                                                  "?type=text&count=-1")))
      (let* ((json-body (cl-json:decode-json-from-string body))
             (message (nmebious::cassoc :message json-body)))
        (is (eql 400
                 code))
        (is (string= message "Count must be a positive number."))))

    ;; incorrect board
    (when (not (nmebious::board-exists-p "thisboarddoesnotexist"))
      (multiple-value-bind (body code headers)
          (dex:get (localhost "api" "posts" "thisboarddoesnotexist"))
        (let* ((json-body (cl-json:decode-json-from-string body))
               (message (nmebious::cassoc :message json-body)))
          (is (eql 400
                   code))
          (is (string= message "Board does not exist.")))))

    ;; incorrect type
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "posts" "?type=incorrect"))
      (let* ((json-body (cl-json:decode-json-from-string body))
             (message (nmebious::cassoc :message json-body)))
        (is (eql 400
                 code))
        (is (string= message "Incorrect type."))))

    ;; submit 1 to each board, check if length 2 with no board specified, check if 1 each
    ;; and their boards are correct in json
    (when (> (length nmebious::*boards*) 0)
      (postmodern:query (:delete-from 'post))
      (let* ((first-board (caar nmebious::*boards*))
             (second-board (caadr nmebious::*boards*)))

        ;; insert random data
        (dotimes (i 2)
          (dexador:post  (localhost "api" "submit" "text")
                         :content (format nil "board=~A&text=~A" first-board i)
                         :headers '((:content-type . "application/x-www-form-urlencoded")))
          (dexador:post  (localhost "api" "submit" "text")
                         :content (format nil "board=~A&text=~A" second-board i)
                         :headers '((:content-type . "application/x-www-form-urlencoded"))))

        ;; check if 2 posts on first board
        (multiple-value-bind (body code headers)
            (dex:get (localhost "api" "posts" (format nil "~A?type=text" first-board)))
          (let* ((json-body (cl-json:decode-json-from-string body)))
            (is (eql (length json-body)
                     1))
            (is (eql (length (nmebious::cassoc :posts
                                               json-body))
                     2))))

        ;; check if 2 posts on second board
        (multiple-value-bind (body code headers)
            (dex:get (localhost "api" "posts" (format nil "~A?type=text" second-board)))
          (let* ((json-body (cl-json:decode-json-from-string body)))
            (is (eql (length json-body)
                     1))
            (is (eql (length (nmebious::cassoc :posts
                                               json-body))
                     2))))

        ;; both together should be 4
        (multiple-value-bind (body code headers)
            (dex:get (localhost "api" "posts" "?type=text"))
          (let* ((json-body (cl-json:decode-json-from-string body)))
            (is (eql (length json-body)
                     1))
            (is (eql (length (nmebious::cassoc :posts
                                               json-body))
                     4))))

        ;; offset works
        (multiple-value-bind (body code headers)
            (dex:get (localhost "api" "posts" "?type=text&offset=1"))
          (let* ((json-body (cl-json:decode-json-from-string body)))
            (is (eql (length json-body)
                     1))
            (is (eql (length (nmebious::cassoc :posts
                                               json-body))
                     3))))

        ;; count works
        (multiple-value-bind (body code headers)
            (dex:get (localhost "api" "posts" "?type=text&count=1"))
          (let* ((json-body (cl-json:decode-json-from-string body)))
            (is (eql (length json-body)
                     1))
            (is (eql (length (nmebious::cassoc :posts
                                               json-body))
                     1))))))))


(test get-server-config
  (with-fixture test-env ()
    ;; check if get server parameters works
    (multiple-value-bind (body code headers)
        (dex:get (localhost "api" "config"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        
        (is (equalp (cl-json::encode-json-alist-to-string (nmebious::cassoc :boards json-body))
                    (cl-json::encode-json-alist-to-string nmebious::*boards*)))
        (is (equalp (nmebious::cassoc :post-get-limit json-body)
                    nmebious::*post-get-limit*))
        (is (equalp (nmebious::cassoc :accepted-mime-types json-body)
                    nmebious::*accepted-mime-types*))
        (is (equalp (nmebious::cassoc :max-file-size json-body)
                    nmebious::*max-file-size*))))))
