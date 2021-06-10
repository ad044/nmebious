(defpackage #:nmebious-test
  (:use #:cl #:fiveam))

(in-package #:nmebious-test)

(def-suite :nmebious)

(in-suite :nmebious)

(def-fixture test-env ()
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
             (postmodern:query (:delete-from 'text-post))
             (postmodern:query (:delete-from 'file-post))
             (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
               (&body)))
        (progn
          (postmodern:disconnect-toplevel)
          (when (hunchentoot:started-p nmebious::*server*)
            (nmebious::stop-hunchentoot))))))

(defun test-file (filename)
  (asdf:system-relative-pathname 'nmebious (format nil "t/~A" filename)))

(defun localhost (&rest path)
  (format nil
          "http://localhost:~A/~{~A~#[~;/~:;/~]~}"
          nmebious::*port*
          path))

(defmacro expect-success ()
  `(is (string-equal (nmebious::cassoc  :status
                                       (cl-json:decode-json-from-string body))
                    "Success"))
  `(is (eql code 200)))

(defmacro expect-error-with-message (message)
  `(let* ((json-body (cl-json:decode-json-from-string body))
                   (status (nmebious::cassoc :status json-body))
                   (message (nmebious::cassoc :message json-body)))
              (is (eql code 400))
              (is (string-equal "Error" status))
              (is (string-equal message ,message))))

(defmacro with-submit-text ((text) &body body)
  `(multiple-value-bind (body code headers uri)
       (dexador:post  (localhost "submit" "txt")
                      :content (cl-json:encode-json-alist-to-string (acons "txt" ,text nil))
                      :headers '((:content-type . "application/json")))
     (declare (ignore headers uri uri))
     ,@body))

(defmacro with-submit-file ((path) &body body)
  `(multiple-value-bind (body code headers uri)
          (dex:post (localhost "submit" "file")
                    :content (acons "photo" (test-file ,path) nil))
        (declare (ignore headers uri))
        ,@body))

(test submit-text
  (with-fixture test-env ()
    (postmodern:query (:delete-from 'text-post))

    ;; random string
    (with-submit-text ("firsttest")
      (expect-success))

    ;; integer
    (with-submit-text (3)
      (expect-success))

    ;; float
    (with-submit-text (10.24)
      (expect-success))

    ;; duplicate checking works (if enabled)
    (when (and nmebious::*allow-duplicates-after*
               (> nmebious::*allow-duplicates-after* 0))
      (with-submit-text ("firsttest")
        (expect-error-with-message "Duplicate post.")))

    ;; text too long
    (with-submit-text ((make-string 256 :initial-element #\a))
      (expect-error-with-message "Text too long."))

    ;; incorrectly named key
    (multiple-value-bind (body code headers uri)
        (dexador:post  (localhost "submit" "txt")
                       :content (cl-json:encode-json-alist-to-string '(("test" . "test")))
                       :headers '((:content-type . "application/json")))
      (declare (ignore headers uri))
      (expect-error-with-message "No text data found in body."))))

(test submit-file
  (with-fixture test-env ()
    (postmodern:query (:delete-from 'file-post))

    ;; jpg image (supported format)
    (when (nmebious::mime-type-accepted-p "image/jpeg")
      (with-submit-file ("test.jpg")
        (expect-success))

      ;; duplicate check (if enabled)
      (when (and nmebious::*allow-duplicates-after*
                 (> nmebious::*allow-duplicates-after* 0))
        (with-submit-file ("test.jpg")
          (expect-error-with-message "Duplicate post."))))

    ;; unsupported format
    (when (not (nmebious::mime-type-accepted-p "image/tiff"))
      (with-submit-file ("test.tiff")
        (expect-error-with-message "Content type not accepted: image/tiff." )))))

(test get-posts
  (with-fixture test-env ()
    ;; insert test data
    (dotimes (i 3)
      (dexador:post  (localhost "submit" "txt")
                     :content (cl-json:encode-json-alist-to-string (acons "txt" i nil))
                     :headers '((:content-type . "application/json"))))
    (dex:post (localhost "submit" "file")
              :content (acons "photo" (test-file "test.jpg") nil))

    ;; retrieving all types of posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "posts" "all/"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        (is (eql (length json-body)
                 2))
        (is (eql (length (nmebious::cassoc :txt
                                           json-body))
                 3))
        (is (eql (length (nmebious::cassoc :file
                                           json-body))
                 1))))

    ;; retrieving only text posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "posts" "txt/"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        (is (eql (length json-body)
                 1))
        (is (eql (length (nmebious::cassoc :txt
                                           json-body))
                 3))))

    ;; retriveing only 2 text posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "posts" "txt" "2"))
      (let* ((json-body (cl-json:decode-json-from-string body)))
        (is (eql (length json-body)
                 1))
        (is (eql (length (nmebious::cassoc :txt
                                           json-body))
                 2))))

    ;; invalid input for post count
    (multiple-value-bind (body code headers)
        (dex:get (localhost "posts" "txt" "test"))
      (let* ((json-body (cl-json:decode-json-from-string body))
             (message (nmebious::cassoc :message json-body)))
        (is (eql 400
                 code))
        (is (string-equal message "Post count must be a number, got: test."))))

    ;; retrieving too many posts
    (multiple-value-bind (body code headers)
        (dex:get (localhost "posts" "txt" (write-to-string (+ 1
                                                              nmebious::*post-get-limit*))))
      (let* ((json-body (cl-json:decode-json-from-string body))
             (message (nmebious::cassoc :message json-body)))
        (is (eql 400
                 code))
        (is (string-equal message "Tried to retrieve too many posts."))))))
