(asdf:defsystem #:nmebious
  :description "Anonymous imageboard"
  :author "ad"
  :license "GPLv3"
  :version "0.4.4"
  :build-operation "program-op"
  :build-pathname "nmebious"
  :entry-point "nmebious::main"
  :serial t
  :depends-on (#:hunchentoot
               #:cl-json
               #:easy-routes
               #:ironclad
               #:metabang-bind
               #:str
               #:postmodern
               #:local-time
               #:cl-postgres+local-time
               #:md5
               #:trivial-mimes
               #:cl-dotenv
               #:swank
               #:xml-emitter
               #:hunchensocket
               #:easy-routes+djula
               #:dexador
               #:quri
               #:hunchentools
               #:trivial-file-size)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "crypto")
                 (:file "config")
                 (:file "db")
                 (:file "nmebious")
                 (:file "views")
                 (:file "decorators")
                 (:file "routes")
                 (:file "sockets"))))
  :in-order-to ((test-op (test-op "nmebious-test"))))
