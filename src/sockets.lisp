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

(defun broadcast (type data board)
  (loop for peer in (hunchensocket:clients *board-listener-instance*)
        do (hunchensocket:send-text-message peer
                                            (encode-json-alist-to-string (pairlis '(type data board)
                                                                                  (list type data board))))))

(defun broadcast-text (text board)
  (broadcast "txt" text board))

(defun broadcast-file (url board)
  (broadcast "file" url board))
