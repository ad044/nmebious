(in-package #:nmebious)

(defvar *server*
  (make-instance 'easy-routes:easy-routes-acceptor
                 :port *port*
                 :document-root nil))

(defvar *socket-handler* nil)

(defun start-db ()
  (set-local-time-cl-postgres-readers)
  (connect-toplevel "nmebious"
                    "nmebious_admin"
                    *db-pass*
                    *db-host*)
  (register-admin-user (parse-envvar "ADMIN_USERNAME")
                       (parse-envvar "ADMIN_PASSWORD")))

(define-static-resource "/static/"
    *static-dir*)
(define-static-resource "/uploads/"
    *uploads-dir*)
(define-static-resource-file "/favicon.ico"
    (merge-pathnames *static-dir* "favicon.ico"))

(defun start-hunchentoot ()
  (start *server*)
  (when *socket-server-enabled-p*
    (setf *socket-handler* (clack:clackup #'socket-server
					  :server :hunchentoot
					  :port 12345))
    (schedule-ping-timer)))

(defun stop-hunchentoot ()
  (stop *server*)
  (when (and *socket-server-enabled-p*
	     *socket-handler*)
    (clack:stop *socket-handler*)
    (unschedule-ping-timer)))

(defun start-server ()
  (start-db)
  (start-hunchentoot))

(defun stop-server ()
  (disconnect-toplevel)
  (stop-hunchentoot))

(defun main (&rest args)
  (declare (ignore args))
  (start-server)
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
