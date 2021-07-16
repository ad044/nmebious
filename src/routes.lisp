(in-package #:nmebious)

;; POST text via API
(defroute api-submit-text ("/api/submit/text" :method :post) ()
  (with-fail-handler (api-submit-text :type 'api)
    (with-submit-text *success*)))

;; POST file via API
(defroute api-submit-file ("/api/submit/file" :method :post) ()
  (with-fail-handler (api-submit-file :type 'api)
    (with-submit-file *success*)))

;; GET posts via API
(defroute get-posts ("/api/posts/:board" :method :get)
    ((type :parameter-type 'string)
     (count :init-form 15 :parameter-type 'integer)
     (offset :init-form 0 :parameter-type 'integer))
  (with-fail-handler (get-posts :type 'api)
    (let* ((board (parse-board-from-req board))
           (ip-hash (hash-ip (real-remote-addr))))
      (with-allowed-check (:ip-hash ip-hash
                           :post-get-count count
                           :type type
                           :board board)
        (encode-json-alist-to-string (pairlis '(posts)
                                              (list (select-posts count
                                                                  offset
                                                                  type
                                                                  board))))))))

;; Server configuration in JSON format
(defroute config ("/api/config" :method :get) ()
  (with-fail-handler (config)
    (encode-json-alist-to-string (pairlis '(boards backgrounds accepted-mime-types post-get-limit max-file-size)
                                          (list  *boards* *backgrounds* *accepted-mime-types* *post-get-limit* *max-file-size*)))))

;; RSS feed
(defroute rss-feed ("/rss" :method :get)
    ((page :init-form 0 :parameter-type 'integer))
  (setf (content-type*) "application/rss+xml")
  (with-fail-handler (rss-feed)
    (let* ((posts (select-posts 30 (* page 30)))
           (sorted-posts (sort posts
                               #'sort-posts-by-id)))
      (with-output-to-string (s)
        (with-rss2 (s :encoding "utf-8")
          (rss-channel-header "nmebious" *web-url*
                              :description "monitoring the wired")
          (dolist (item sorted-posts)
            (let* ((data (cassoc :data item))
                   (id (cassoc :id item))
                   (board (cassoc :board item))
                   (date (cassoc :submission-date item)))
              (rss-item nil
                        :guid id
                        :category board
                        :description data
                        :pubDate date))))))))

;; POST file via the default frontend
(defroute web-submit-file ("/web-view/submit/file" :method :post) ()
  (with-fail-handler (web-submit-file :type 'web-view)
    (with-submit-file
      (redirect-back-to-board))))

;; POST text via the default frontend
(defroute web-submit-text ("/web-view/submit/text" :method :post) ()
  (with-fail-handler (web-submit-text :type 'web-view)
    (with-submit-text
      (redirect-back-to-board))))

;; View for a board
(defroute board-posts ("/boards/:board" :method :get) ()
  (with-fail-handler (board-posts :type 'web-view)
    (setf (content-type*) "text/html; charset=utf-8")
    (hunchentoot:start-session)
    (let* ((flash-message (hunchentoot:session-value :flash-message)))
      (setf (hunchentoot:session-value :flash-message) nil)
      (with-allowed-check (:board board)
        (setf (hunchentoot:session-value :board) board)
        (render-board board :error flash-message)))))
