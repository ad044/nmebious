(defpackage #:nmebious
  (:use #:cl)
  (:shadowing-import-from #:hunchensocket
                          #:websocket-resource
                          #:websocket-client
                          #:*websocket-dispatch-table*
                          #:clients
                          #:send-text-message
                          #:send-binary-message)
  (:shadowing-import-from #:xml-emitter
                          #:with-rss2
                          #:rss-channel-header
                          #:rss-item)
  (:shadowing-import-from #:cl-dotenv
                          #:read-env)
  (:shadowing-import-from #:trivial-mimes
                          #:mime-file-type)
  (:shadowing-import-from #:cl-json
                          #:decode-json-from-string
                          #:encode-json-alist-to-string)
  (:shadowing-import-from #:local-time
                          #:universal-to-timestamp
                          #:timestamp<
                          #:format-timestring
                          #:set-local-time-cl-postgres-readers
                          #:now)
  (:shadowing-import-from #:md5
                          #:md5sum-file
                          #:md5sum-string)
  (:shadowing-import-from #:postmodern
                          #:query
                          #:sql-compile
                          #:connect-toplevel
                          #:disconnect-toplevel)
  (:shadowing-import-from #:str
                          #:split)
  (:shadowing-import-from #:uiop
                          #:run-program)
  (:shadowing-import-from #:metabang-bind
                          #:bind)
  (:shadowing-import-from #:ironclad
                          #:ascii-string-to-byte-array
                          #:hmac-digest
                          #:update-hmac
                          #:make-hmac
                          #:byte-array-to-hex-string
                          #:random-data)
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
                          #:script-name
                          #:redirect
                          #:*session*
                          #:session-value)
  (:shadowing-import-from #:easy-routes
                          #:defroute)
  (:shadowing-import-from #:djula
                          #:add-template-directory
                          #:compile-template*
                          #:render-template*)
  (:export #:start-server
           #:stop-server
           #:*port*))


(in-package :cl-json)
(defmethod encode-json ((o local-time:timestamp)
                        &optional (stream json:*json-output*))
  (encode-json (local-time:format-timestring nil o) stream))
