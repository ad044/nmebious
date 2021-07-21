(in-package #:nmebious)

(defun @check-frontend-enabled (next)
  (if *default-frontend-enabled-p*
      (funcall next)
      (render-404)))

(defun @check-banned (next)
  (if (banned-p (hash-ip (real-remote-addr)))
      (progn
        (setf (session-value :flash-message) "You are banned.")
        (redirect-back-to-board))
      (funcall next)))

(defun @html (next)
  (setf (content-type*) "text/html")
  (funcall next))

(defun @json (next)
  (setf (content-type*) "application/json")
  (funcall next))

(defun @rss (next)
  (setf (content-type*) "application/rss+xml")
  (funcall next))
