(in-package #:nmebious)

;; POST text
(defroute submit-text ("/submit/txt" :method :post) ()
  (with-fail-handler (submit-text)
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
    (let* ((json (decode-json-from-string (raw-post-data :force-text t)))
           (ip-hash (hash-ip  (real-remote-addr)))
           (board (or (cassoc :board json)
                      (throw-request-error "No board data found.")))
           (submitted-text (or (cassoc :txt json)
                               (throw-request-error "No text data found in body.")))
           (formatted-text (format nil "~A" submitted-text)))
      (with-allowed-check (:board board
                           :ip-hash ip-hash
                           :text-data formatted-text)
        (let* ((post-id (caar (insert-text-row board
                                               formatted-text
                                               ip-hash))))
          (broadcast :type "txt"
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
          (let* ((post-id (caar (insert-file-row board
                                                 full-filename
                                                 checksum
                                                 ip-hash))))
            (broadcast :type "file"
                       :post-id post-id
                       :data full-filename
                       :board board)
            *success*))))))

;; GET posts
(defroute get-posts ("/posts/:board" :method :get)
    ((type :init-form "all")
     (count :init-form 15 :parameter-type 'integer)
     (offset :init-form 0 :parameter-type 'integer))
  (with-fail-handler (get-posts)
    (let* ((table (get-table-for-type type))
           (board (parse-board-from-req board))
           (ip-hash (hash-ip (real-remote-addr))))
      (with-allowed-check (:ip-hash ip-hash
                           :post-get-count count)
        (encode-json-alist-to-string (select-posts count :table table
                                                         :board board
                                                         :offset offset))))))

;; RSS feed
(defroute rss-feed ("/rss" :method :get) ()
  (setf (content-type*) "application/rss+xml")
  (with-fail-handler (rss-feed)
    (let* ((posts (select-posts 30))
           (text-posts (cdar posts))
           (file-posts (cdadr posts))
           (all-posts (append text-posts
                              file-posts))
           (sorted-all-posts (sort all-posts
                                   #'sort-posts-by-date)))
      (with-output-to-string (s)
        (with-rss2 (s :encoding "utf-8")
          (rss-channel-header "nmebious" *web-url*
                              :description "monitoring the wired")
          (dolist (item sorted-all-posts)
            (let* ((text-data (cassoc :text-data item))
                   (file-data (cassoc :filename item)))
              (rss-item nil
                        :link (format-image-link file-data)
                        :description text-data
                        :pubDate (format-timestring nil (cassoc :submission-date item))))))))))

(defroute boards ("/boards" :method :get) ()
  (encode-json-alist-to-string (acons 'boards *boards* nil)))
