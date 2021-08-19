(in-package #:nmebious)

(defclass board-listener (websocket-resource) ()
  (:default-initargs :client-class 'user))

(defclass user (websocket-client) ())

(defparameter *board-listener-instance*
  (make-instance 'board-listener))

(defun connect (request)
  (when (string= (script-name request)
                 "/ws")
    *board-listener-instance*))

(pushnew 'connect *websocket-dispatch-table*)

(defun broadcast (&key type post-id data board)
  (mapcar #'(lambda (peer)
              (send-text-message peer
               (encode-json-alist-to-string
                (pairlis '(type id data board)
                         (list type post-id data board)))))
          (clients *board-listener-instance*)))

(defun send-ping ()
  (mapcar #'(lambda (peer)
              (hunchensocket::send-frame peer #x9 nil))
          (clients *board-listener-instance*)))

(defparameter *ping-timer*
  (sb-ext:make-timer #'send-ping :thread t))

(defun schedule-ping-timer ()
  (sb-ext:schedule-timer *ping-timer* 20 :repeat-interval 20))

(defun unschedule-ping-timer ()
  (sb-ext:unschedule-timer *ping-timer*))


;; Without having these defined, upon receiving a message it seems to throw an error.
;; So we define it, but leave it empty, since we don't do anything with this data.
(defmethod hunchensocket:text-message-received ((listener board-listener) user message))

(defmethod hunchensocket:binary-message-received ((listener board-listener) user message))
