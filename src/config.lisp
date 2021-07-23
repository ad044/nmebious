(in-package #:nmebious)

;; Which port to run hunchentoot on
(defparameter *port* 8080)

;; Make the API accessible to anyone or require a key.
(defparameter *api-requires-key* nil)

;; Whether or not to enable socket server for front-ends that support live updates
(defparameter *socket-server-enabled-p* t)

;; Accepted mime types for file submissions
(defparameter *accepted-mime-types* '("image/png" "image/jpeg" "image/gif"))

;; Boards (must be at least 1)
;; For each board you can specify the name, the background, and the color.
;; If you don't want to have a background, set it to NIL.
;; If you don't want a custom color and want the default greenish look, set the color attribute to NIL.
(defparameter *boards* '(("main" . ((:background . "main.jpg") (:color . nil)))
                         ("second" . ((:background . nil) (:color . "#ffffff")))))

;; Publicly served static files
(defparameter *static-dir* (asdf:system-relative-pathname 'nmebious "static/"))

;; Where to store user uploaded content
(defparameter *uploads-dir* (asdf:system-relative-pathname 'nmebious "uploads/"))

;; URL for the website
(defparameter *web-url* "https://example.website/")

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


;; ==================       Configuration for the default frontend (if enabled)        ======================

;; Enable/disable default frontend
(defparameter *default-frontend-enabled-p* t)

;; How many file entries to display on page
(defparameter *file-display-count* 10)

;; How many text entries to display on page
(defparameter *text-display-count* 20)

;; Enable/disable pagination to allow users to view past posts
(defparameter *pagination-on-default-frontend-enabled-p* t)

;; User preferences for the frontend
(defparameter *web-user-preferences* `((:icons . ((:description . "Display icons (top right corner) on the main page")
                                                  (:default . t)
                                                  (:depends-on . t)))
                                       (:pagination . ((:description . "Display pagination buttons")
                                                       (:default . t)
                                                       (:depends-on . ,*pagination-on-default-frontend-enabled-p*)))
                                       (:boardlist . ((:description . "Display board list in the bottom middle of the page")
                                                      (:default . t)
                                                      (:depends-on . ,(not (single-board-p)))))
                                       (:backgrounds . ((:description . "Display custom backgrounds for boards (if set by the instance)")
                                                        (:default . nil)
                                                        (:depends-on . ,(instance-has-backgrounds-p))))))
