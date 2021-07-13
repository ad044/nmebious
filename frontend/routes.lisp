(in-package #:nmebious.frontend)

;; Boards
(defroute board-posts ("/boards/:board" :method :get) ()
  (setf (content-type*) "text/html; charset=utf-8")
  (let* ((text-posts (nmebious::cassoc :posts
                                       (get-posts board "text" *text-display-count*)))
         (file-posts (nmebious::cassoc :posts
                                        (get-posts board "file" *file-display-count*))))
    (djula:render-template* +mebious.html+ nil
                            :text-posts text-posts
                            :file-posts file-posts
                            :api-url *api-url*)))


;; RSS feed
(defroute rss-feed ("/rss" :method :get)
    ((page :init-form 0 :parameter-type 'integer))
  (setf (content-type*) "application/rss+xml")
  (let* ((rss-data (nmebious::cassoc :posts (get-rss-feed page))))
    (with-output-to-string (s)
      (with-rss2 (s :encoding "utf-8")
        (rss-channel-header "nmebious" *web-url*
                            :description "monitoring the wired")
        (dolist (item rss-data)
          (let* ((data (nmebious::cassoc :data item))
                 (id (nmebious::cassoc :id item))
                 (board (nmebious::cassoc :board item))
                 (date (nmebious::cassoc :submission-date item)))
            (rss-item nil
                      :guid id
                      :category board
                      :description data
                      :pubDate date)))))))
