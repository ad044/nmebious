(in-package #:nmebious)

(defun @check-banned (next)
  (if (banned-p (hash-ip (real-remote-addr)))
      (throw-request-error "You are banned.")
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
