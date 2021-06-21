(in-package #:nmebious)

(defclass board-listener (hunchensocket:websocket-resource) ()
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client) ())

(defparameter *board-listener-instance* (make-instance 'board-listener))

(defun connect (request)
  (when (string= (hunchentoot:script-name request)
                 "/")
    *board-listener-instance*))

(pushnew 'connect hunchensocket:*websocket-dispatch-table*)

(defun broadcast (instance message)
  (loop for peer in (hunchensocket:clients *board-listener-instance*)
        do (hunchensocket:send-text-message peer
                                            message)))

(defmethod hunchensocket:text-message-received ((instance board-listener) user message)
  (broadcast instance message))
