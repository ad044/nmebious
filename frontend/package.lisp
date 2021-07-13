(defpackage #:nmebious.frontend
  (:use #:cl)
  (:shadowing-import-from #:xml-emitter
                          #:with-rss2
                          #:rss-channel-header
                          #:rss-item)
  (:shadowing-import-from #:hunchentoot
                          #:start
                          #:stop
                          #:raw-post-data
                          #:real-remote-addr
                          #:post-parameters*
                          #:return-code*
                          #:+http-bad-request+
                          #:*default-content-type*
                          #:content-type*
                          #:header-out
                          #:script-name)
  (:shadowing-import-from #:dexador
                          #:get)
  (:shadowing-import-from #:easy-routes
                          #:defroute)
  (:shadowing-import-from #:cl-json
                          #:decode-json-from-string)
  (:export #:start-frontend-server
           #:stop-frontend-server))
