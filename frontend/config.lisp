(in-package #:nmebious.frontend)

;; Which port to run the hunchentoot server on
(defparameter *port* 3001)

;; How many file entries to display on page
(defparameter *file-display-count* 10)

;; How many text entries to display on page
(defparameter *text-display-count* 20)

;; URL for the front-end website (the "/" at the end is important)
(defparameter *web-url* "https://example.website/")

;; URL for the backend API (the "/" at the end is important) (defaults to localhost)
(defparameter *api-url* (format nil "http://localhost:~A/" nmebious:*port*))
