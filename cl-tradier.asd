(in-package #:cl-user)

(defpackage #:cl-tradier-asd
  (:use #:cl #:asdf))

(in-package #:cl-tradier-asd)

(defsystem #:cl-tradier
  :serial t
  :description "Unofficial Common Lisp thin client for the Tradier brokerage restful API."
  :author "William Bruschi"
  :license "MIT"
  :depends-on (#:cl-ppcre
	       #:alexandria
	       #:drakma)
  :components ((:file "package")
               (:file "cl-tradier")
	       (:file "conditions")
	       (:file "tradier-request")
	       (:file "define-tradier-endpoint")
	       (:file "endpoints")
	       (:file "api")))

(defsystem #:cl-tradier-test
  :components ((:module "test"
		:serial t
		:components ((:file "package")
			     (:file "tradier-test-session")
			     (:file "test-records")
			     (:file "tests"))))
  :depends-on (#:cl-tradier #:fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run! :cl-tradier-test-suite)))
