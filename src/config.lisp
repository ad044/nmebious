(in-package #:nmebious)

;;  ====================== EDIT THIS  ================
(defvar *config* nil)
;;  ==================================================

;; Env file
;; (Inside a docker container it won't exist since all env vars will be located inside memory, so we check for that.)
(defparameter *env*
  (let ((dotenv-path (asdf:system-relative-pathname 'nmebious ".env")))
    (when (probe-file dotenv-path)
      (read-env dotenv-path))))

;; Helper for parsing envvars and throwing warnings if they're not found
(defun parse-envvar (envvar)
  (flet ((numeric-string-p
	     (string)
	   (ignore-errors (parse-integer string))))
    (handler-case 
	(let ((envvar-value (or (sb-ext:posix-getenv envvar)
				(gethash envvar *env*))))
	  (if (numeric-string-p envvar-value)
	      (parse-integer envvar-value)
	      envvar-value))
      (error ()
	(format t
		"~%WARNING: Missing envvar ~A. The server will not function properly!~%"
		envvar)))))

;; Macro for defining configs in a more syntactically pleasing way
;; yields a fset:map with the key-val pairs.
(defmacro construct-config (&rest slot-values)
  `(fset:map ,@(loop for (a b)
		       on slot-values
		     by #'cddr collect (list a b))))

;; Macro for defining new configurations immutably.
;; Example: 
;; (extend-config *default-config* :web-url "new-url")
;; yields the default configuration with the `web-url` property changed to be "new-url"
(defmacro extend-config (config &rest slot-values)
  `(fset:map-union ,config
		   (construct-config ,@slot-values)))

;; Default configuration. Do not modify this (tests depend on the default configuration,
;; so modifying it will break them).
(defvar *default-config*
  (construct-config
   ;; Which port to run hunchentoot on
   :port 8080

   ;; Make the API accessible to anyone or require a key.
   :api-requires-key nil
  
   ;; Whether or not to enable socket server for front-ends that support live updates
   :socket-server-enabled-p nil
  
   ;; Accepted mime types for file submissions
   :accepted-mime-types '("image/png" "image/jpeg" "image/gif")

   ;; Boards (must be at least 1)
   ;; For each board you can specify the name, the background, and the color.
   ;; If you don't want to have a background, set it to NIL.
   ;; If you don't want a custom color and want the default greenish look, set the color attribute to NIL.
   :boards '(("main" . ((:background . nil)
			(:color . nil)))
	     ("secondary" . ((:background . nil)
			     (:color . "#ffffff"))))

   ;; Public files
   :static-dir (asdf:system-relative-pathname 'nmebious "public/")

   ;; Where to store user uploaded content
   :uploads-dir (asdf:system-relative-pathname 'nmebious "public/uploads/")

   ;; URL for the website
   :web-url "https://example.website/"

   ;; Limit how many posts a user can retreive at once using the API
   :post-get-limit 1000

   ;; After how many posts can a user post a duplicate
   ;; Set to NIL to disable duplicate checking
   :allow-duplicates-after 5

   ;; Posts containing these words will get rejected
   :filtered-words '()
  
   ;; Postgres host
   ;; If ran by docker, the environment variable DB_HOST will be attached to the DB service.
   ;; If it's not attached, we're running locally, therefore the host is localhost.
   :db-host (or (sb-ext:posix-getenv "DB_HOST")
		"localhost")
  
   ;; Postgres password
   :db-pass (parse-envvar "POSTGRES_PASSWORD")

   ;; Admin panel username
   :admin-username (parse-envvar "ADMIN_USERNAME")

   ;; Admin panel pass
   :admin-pass (parse-envvar "ADMIN_PASSWORD")
  
   ;; Maximum file size (in mbs)
   :max-file-size (parse-envvar "MAX_FILE_SIZE")

   ;; Enable-disable default frontend
   :default-frontend-enabled-p t
  
   ;; =========  Configuration for the default frontend (if enabled)   ======================

   ;; Fonts
   :fonts '("Times New Roman" "Times" "serif" "Arial"
	    "Helvetica" "sans-serif" "Georgia" "Courier New"
	    "Courier" "monospace")

   ;; How many file entries to display on page
   :file-display-count 10

   ;; How many text entries to display on page
   :text-display-count 20

   ;; Enable/disable pagination to allow users to view past posts
   :pagination-on-default-frontend-enabled-p t

   ;; Default user preferences for the frontend
   :web-user-preferences
   (list
    (cons :icons 
	  `((:description . "Display icons (top right corner) on the main page")
	    (:default . t)))
    (cons :backgrounds
	  `((:description . "Display custom backgrounds for boards (if set by the instance)")
	    (:default . nil))))))

;; Utils for configuration
(defun get-config (keyword)
  (fset:lookup (or *config*
	      *default-config*)
	  keyword))

(defun load-config (config)
  (setf *config* config))

(defun unload-config ()
  (setf *config* nil))
