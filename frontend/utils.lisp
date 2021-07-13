(in-package #:nmebious.frontend)

(defun format-get-url (board type count)
  (format nil "~Aposts/~A?type=~A&count=~A" *api-url* board type count))

(defun format-rss-url (page)
  (format nil "~Arss-data?page=~A" *api-url* page))

(defun get-posts (board type count)
  (multiple-value-bind (body code headers)
      (get (format-get-url board type count))
    (decode-json-from-string body)))

(defun get-rss-feed (page)
  (multiple-value-bind (body code headers)
      (get (format-rss-url page))
    (decode-json-from-string body)))
