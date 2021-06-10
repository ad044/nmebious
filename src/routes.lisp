(in-package #:nmebious)

;; POST text
(defroute submit-text ("/submit/txt" :method :post) ()
  (with-fail-handler (submit-text)
    (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
    (let* ((json (decode-json-from-string (raw-post-data :force-text t)))
           (ip-hash (hash-ip  (real-remote-addr)))
           (submitted-text (or (cassoc :txt json)
                               (throw-request-error "No text data found in body.")))
           (formatted-text (format nil "~A" submitted-text)))
      (with-allowed-check (:ip-hash ip-hash
                           :text-data formatted-text)
        (insert-text-row formatted-text ip-hash)
        *success*))))

;; POST file
(defroute submit-file ("/submit/file" :method :post) ()
  (with-fail-handler (submit-file)
    (bind (((name src filename content-type) (car (post-parameters*)))
           (ip-hash (hash-ip (real-remote-addr)))
           (checksum (hex (md5sum-file src))))
          (with-allowed-check (:ip-hash ip-hash
                               :checksum checksum
                               :content-type content-type)
            (let* ((filename (gen-filename))
                   (type (mime-file-type content-type))
                   (full-filename (concatenate 'string filename "." type))
                   (dest (make-pathname :directory (append (pathname-directory *static-dir*))
                                        :name filename
                                        :type type)))
              (format-and-save-file src
                                    dest)
              (insert-file-row full-filename checksum ip-hash)
              *success*)))))

;; GET posts
(defroute get-posts ("/posts/:type/:n" :method :get) ()
  (with-fail-handler (get-posts)
    (let* ((table (get-table-for-type type))
           (count (parse-get-count-from-string n))
           (ip-hash (hash-ip (real-remote-addr))))
      (with-allowed-check (:ip-hash ip-hash
                           :post-get-count count)
        (encode-json-alist-to-string (select-posts count table))))))

;; RSS feed
(defroute rss-feed ("/rss" :method :get) ()
  (setf (content-type*) "application/rss+xml")
  (let* ((posts (select-posts 30 nil))
         (text-posts (cdar posts))
         (file-posts (cdadr posts))
         (all-posts (reduce #'cons
                            text-posts
                            :initial-value file-posts
                            :from-end t))
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
                      :pubDate (format-timestring nil (cassoc :submission-date item)))))))))
