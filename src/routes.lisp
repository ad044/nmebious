(in-package #:nmebious)

;; POST text
(defroute submit-text ("/submit/text" :method :post) ()
  (with-fail-handler (submit-text)
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
    (let* ((json (decode-json-from-string (raw-post-data :force-text t)))
           (ip-hash (hash-ip  (real-remote-addr)))
           (board (or (cassoc :board json)
                      (throw-request-error "No board data found.")))
           (submitted-text (or (cassoc :text json)
                               (throw-request-error "No text data found in body.")))
           (formatted-text (format nil "~A" submitted-text))
           (checksum (hex (md5sum-string formatted-text))))
      (with-allowed-check (:board board
                           :ip-hash ip-hash
                           :text-data formatted-text
                           :checksum checksum)
        (let* ((post-id (caar (insert-post formatted-text
                                           checksum
                                           "text"
                                           board
                                           ip-hash))))
          (broadcast :type 'text
                     :post-id post-id
                     :data formatted-text
                     :board board)
          *success*)))))

;; POST file
(defroute submit-file ("/submit/file" :method :post) ()
  (with-fail-handler (submit-file)
    (bind (((src filename content-type) (or (cassoc "file"
                                                        (post-parameters*)
                                                        :test #'string-equal)
                                                 (throw-request-error "No file data found.")))
            (board (or (cassoc "board"
                               (post-parameters*)
                               :test #'string-equal)
                       (throw-request-error "No board data found.")))
            (ip-hash (hash-ip (real-remote-addr)))
            (checksum (hex (md5sum-file src))))
      (with-allowed-check (:board board
                           :ip-hash ip-hash
                           :checksum checksum
                           :content-type content-type)
        (let* ((filename (gen-filename))
               (type (mime-file-type content-type))
               (full-filename (concatenate 'string filename "." type))
               (dest (make-pathname :directory (pathname-directory *static-dir*)
                                    :name filename
                                    :type type)))
          (format-and-save-file src
                                dest)
          (let* ((post-id (caar (insert-post full-filename
                                             checksum
                                             "file"
                                             board
                                             ip-hash))))
            (broadcast :type 'file
                       :post-id post-id
                       :data full-filename
                       :board board)
            *success*))))))

;; GET posts
(defroute get-posts ("/posts/:board" :method :get)
    ((type :parameter-type 'string)
     (count :init-form 15 :parameter-type 'integer)
     (offset :init-form 0 :parameter-type 'integer))
  (with-fail-handler (get-posts)
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
            (let* ((text-data (cassoc :text-data item))
                   (file-data (cassoc :filename item))
                   (id (cassoc :id item))
                   (board (cassoc :board item)))
              (rss-item nil
                        :guid id
                        :category board
                        :link (format-image-link file-data)
                        :description text-data
                        :pubDate (format-timestring nil (cassoc :submission-date item))))))))))

;; Server configuration
(defroute config ("/config" :method :get) ()
  (with-fail-handler (config)
    (encode-json-alist-to-string (pairlis '(boards backgrounds accepted-mime-types post-get-limit max-file-size)
                                          (list  *boards* *backgrounds* *accepted-mime-types* *post-get-limit* *max-file-size*)))))
