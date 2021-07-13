(in-package #:nmebious)

;; Which port to run hunchentoot on
(defparameter *port* 8080)

;; Accepted mime types for file submissions
(defparameter *accepted-mime-types* (list "image/png"
                                          "image/jpeg"
                                          "image/gif"))

;; Boards (must be at least 1)
(defparameter *boards* (list "main" "second"))

;; Publically served static files
(defparameter *static-dir* (asdf:system-relative-pathname 'nmebious "static/"))

;; Where to store user uploaded content
(defparameter *uploads-dir* (asdf:system-relative-pathname 'nmebious "static/uploads/"))

;; Background for each board (simply put filenames in the same order as *boards*)
;; If you don't want a specific board to have a background, just put NIL at that point
;; If you don't want any backgrounds, just set this parameter to NIL
;; These files should be put inside static/bg/<filename>, where the filenames are the entries
;; from this list
(defparameter *backgrounds* (list "main.jpg" "second.jpg"))

;; Limit how many posts a user can retreive at once using the API
(defparameter *post-get-limit* 1000)

;; After how many posts can a user post a duplicate
;; Set to NIL to disable duplicate checking
(defparameter *allow-duplicates-after* 5)

;; Env file
(defparameter *env* (read-env (asdf:system-relative-pathname 'nmebious ".env")))

;; Postgres user
(defparameter *db-user* (gethash "PGUSER" *env*))

;; Postgres password
(defparameter *db-pass* (gethash "PGPASSWORD" *env*))

;; HMAC secret
(defparameter *secret* (gethash "SECRET" *env*))

;; Maximum file size (in mbs)
(defparameter *max-file-size* (parse-integer (gethash "MAX_FILE_SIZE" *env*)))
