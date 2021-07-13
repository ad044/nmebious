(in-package #:nmebious.frontend)

(defparameter *server* (make-instance 'easy-routes:easy-routes-acceptor
                                      :port *port*
                                      :document-root nil))

(djula:add-template-directory (asdf:system-relative-pathname "nmebious" "frontend/templates/"))

(defparameter +mebious.html+ (djula:compile-template* "mebious.html"))

(defun start-frontend-server ()
  (start *server*))

(defun stop-frontend-server ()
  (stop *server*))
