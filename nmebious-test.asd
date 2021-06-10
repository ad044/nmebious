(asdf:defsystem #:nmebious-test
  :description "Test suite for nmebious"
  :author "ad"
  :license "GPLv3"
  :version "0.4.4"
  :depends-on (:fiveam :dexador :nmebious)
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (o c)
                    (symbol-call :5am :run! :nmebious)))
