(in-package #:nmebious)

(defvar *server*
  (make-instance 'easy-routes:easy-routes-acceptor
                 :port (get-config :port)
                 :document-root nil))

(defvar *database-connected-p* nil)

(defvar *socket-handler* nil)

(defun start-db (&optional (db "nmebious") (user "nmebious_admin"))
  (set-local-time-cl-postgres-readers)
  (connect-toplevel db
		    user
		    (get-config :db-pass)
		    (get-config :db-host))
  (setf *database-connected-p* t)
  (register-admin-user (get-config :admin-username) 
                       (get-config :admin-pass)))

(define-static-resource "/static/"
    (get-config :static-dir))
(define-static-resource "/uploads/"
    (get-config :uploads-dir))
(define-static-resource-file "/favicon.ico"
    (merge-pathnames (get-config :static-dir) "favicon.ico"))

(defun start-hunchentoot ()
  (start *server*)
  (when (get-config :socket-server-enabled-p)
    (setf *socket-handler* (clack:clackup #'socket-server
					  :server :hunchentoot
					  :port 12345))
    (schedule-ping-timer)))

(defun stop-hunchentoot ()
  (stop *server*)
  (when (and (get-config :socket-server-enabled-p)
	     *socket-handler*)
    (clack:stop *socket-handler*)
    (unschedule-ping-timer)))

(defun start-server (&optional (config *default-config*))
  (load-config config)
  (unless *database-connected-p*
    (start-db))
  (unless (started-p *server*)
    (start-hunchentoot)))

(defun stop-server ()
  (when *database-connected-p*
    (disconnect-toplevel)
    (setf *database-connected-p* nil))
  (when (started-p *server*)
    (stop-hunchentoot)))

(defun main (&rest args)
  (declare (ignore args))
  (start-server
   (or *config*
       (progn 
	 (format t "~%WARNING: No custom configuration found, falling back to default.~%")
	 *default-config*)))
  (setf swank::*loopback-interface* "0.0.0.0")
  (swank-loader:init)
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)
  (handler-case
      (bt:join-thread
       (find-if (lambda (th)
                  (search "hunchentoot" (bt:thread-name th)))
                (bt:all-threads)))
    (sb-sys:interactive-interrupt ()
      (progn
        (format *error-output* "Aborting.~&")
        (stop-server)
        (uiop:quit)))
    (error (c)
      (format t "Woops, an unknown error occured:~&~a~&" c))))
