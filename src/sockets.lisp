(in-package #:nmebious)

(defvar *connections* (make-hash-table))

(defun handle-close-connection (connection)
  (remhash connection *connections*))

(defun handle-new-connection (connection)
  (setf (gethash connection *connections*)
        (format nil "user-~a" (random 100000))))

(defun broadcast-to-room (&key type post-id data board)
  (let ((message (encode-json-alist-to-string
                  (pairlis '(type id data board)
                           (list type post-id data board)))))
    (loop :for con :being :the :hash-key :of *connections* :do
      (send con message))))

(defun socket-server (env)
  (let ((ws (make-server env)))
    (on :open ws
        (lambda ()
	  (handle-new-connection ws)))
    (on :close ws
        (lambda (&key code reason)
          (declare (ignore code reason))
          (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (start-connection ws))))

(defun ping-all-connections ()
  (loop :for con :being :the :hash-key :of *connections* :do
    (send-ping con)))

(defvar *ping-timer*
  (sb-ext:make-timer #'ping-all-connections :thread t))

(defun schedule-ping-timer ()
  (sb-ext:schedule-timer *ping-timer* 20 :repeat-interval 20))

(defun unschedule-ping-timer ()
  (sb-ext:unschedule-timer *ping-timer*))
