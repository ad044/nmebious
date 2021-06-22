(in-package #:nmebious)

;; Which port to run hunchentoot on
(defparameter *port* 8080)

;; URL for the website (the "/" at the end is important)
(defparameter *web-url* "https://example.website/")

;; Accepted mime types for file submissions
(defparameter *accepted-mime-types* (list "image/png"
                                          "image/jpeg"
                                          "image/gif"))

;; Boards (must be at least 1)
(defparameter *boards* (list "main" "second"))

;; Where to store user uploaded content
(defparameter *static-dir* (asdf:system-relative-pathname 'nmebious "static/"))

;; Limit how many posts a user can retreive at once using the API
(defparameter *post-get-limit* 50)

;; After how many posts can a user post a duplicate
;; Set to NIL to disable duplicate checking
(defparameter *allow-duplicates-after* nil)

;; Env file
(defparameter *env* (read-env (asdf:system-relative-pathname 'nmebious ".env")))

;; Postgres user
(defparameter *db-user* (gethash "DB_USER" *env*))

;; Postgres password
(defparameter *db-pass* (gethash "DB_PASS" *env*))

;; HMAC secret
(defparameter *secret* (gethash "SECRET" *env*))
