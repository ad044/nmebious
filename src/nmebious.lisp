(in-package #:nmebious)

(defparameter *server* (make-instance 'easy-routes:easy-routes-acceptor
                                      :port *port*
                                      :document-root nil))

(defvar *socket-server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))

(defun start-db ()
  (set-local-time-cl-postgres-readers)
  (connect-toplevel "nmebious"
                    *db-user*
                    *db-pass*
                    "localhost")
  (setup-db))

(define-static-resource "/static/" *static-dir*)
(define-static-resource *uploads-web-path* *uploads-dir*)

(defun start-hunchentoot ()
  (setf *default-content-type* "application/json")
  (start *server*)
  (when *socket-server-enabled-p*
    (start *socket-server*))
  (schedule-ping-timer))

(defun stop-hunchentoot ()
  (stop *server*)
  (when *socket-server-enabled-p*
    (stop *socket-server*))
  (unschedule-ping-timer))

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

