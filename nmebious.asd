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
               #:xml-emitter)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config")
                 (:file "db")
                 (:file "utils")
                 (:file "nmebious")
                 (:file "routes"))))
  :in-order-to ((test-op (test-op "nmebious-test"))))
