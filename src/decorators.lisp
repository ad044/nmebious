(in-package #:nmebious)

(defun @check-api-key (next)
  (let* ((api-key (cassoc "api-key" (post-parameters*) :test #'string=)))
    (if *api-requires-key*
        (cond ((not api-key)
               (api-fail-with-message "Must provide an API key."))
              ((not (api-key-valid-p api-key))
               (api-fail-with-message "Invalid API key."))
              (t (funcall next)))
        (funcall next))))

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

(defun @is-admin (next)
  (if (session-value :is-admin)
      (funcall next)
      (redirect "/admin")))

(defun @html (next)
  (setf (content-type*) "text/html")
  (funcall next))

(defun @json (next)
  (setf (content-type*) "application/json")
  (funcall next))

(defun @rss (next)
  (setf (content-type*) "application/rss+xml")
  (funcall next))
