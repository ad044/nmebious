(in-package #:nmebious)

(defun @check-api-key (next)
  (let* ((api-key (cassoc "api-key" (post-parameters*) :test #'string=)))
    (if *api-requires-key*
        (cond ((not api-key)
               (api-fail-with-message "Must provide an API key." 400))
              ((not (api-key-valid-p api-key))
               (api-fail-with-message "Invalid API key." 401))
              (t (funcall next)))
        (funcall next))))

(defun @check-frontend-enabled (next)
  (if *default-frontend-enabled-p*
      (funcall next)
      (render-error-page "This instance does not have the default frontend enabled." 404)))

(defun @is-admin (next)
  (if (session-value :is-admin)
      (funcall next)
      (render-error-page "You cannot access this page." 403)))

(defun @html (next)
  (setf (content-type*) "text/html")
  (funcall next))

(defun @json (next)
  (setf (content-type*) "application/json")
  (funcall next))

(defun @rss (next)
  (setf (content-type*) "application/rss+xml")
  (funcall next))
