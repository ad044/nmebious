(in-package #:nmebious)

(defparameter *server* (make-instance 'easy-routes:easy-routes-acceptor
                                      :port *port*
                                      :document-root nil))

(defun start-db ()
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" *static-dir*)
        hunchentoot:*dispatch-table*)
  (set-local-time-cl-postgres-readers)
  (connect-toplevel "nmebious"
                    nmebious::*db-user*
                    nmebious::*db-pass*
                    "localhost")
  (setup-db))

(defun start-hunchentoot ()
  (setf *default-content-type* "application/json")
  (start *server*))

(defun stop-hunchentoot ()
  (stop *server*))

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
  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    (sb-sys:interactive-interrupt
     () (progn
          (format *error-output* "Aborting.~&")
          (stop-server)
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))

