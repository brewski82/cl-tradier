(in-package #:cl-tradier-test)

(defclass test-tradier-session (tradier-session) ())

(initialize-tradier-session :environment :prod :access-token "access-token" :tradier-session-class 'test-tradier-session)

(defstruct test-record test-function test-paramaters url method query-string response)

(defparameter *test-records* nil)

(defmacro define-test-records (&body body)
  `(progn
     (setf *test-records* nil)
     ,@(loop for (test-function test-paramaters url method query-string response) in body
	     collect `(push (make-test-record :test-function ,test-function
					      :test-paramaters ,test-paramaters
					      :url ,url
					      :method ,method
					      :query-string ,query-string
					      :response ,response)
			    *test-records*))))

(defmethod tradier-http-request ((tradier-session test-tradier-session) (url string) &key method parameters &allow-other-keys)
  (loop for test-record in *test-records*
	for test-url = (test-record-url test-record)
	for test-method = (test-record-method test-record)
	when (and (string-equal url test-url) (equalp method test-method))
	  do (return-from tradier-http-request (values (test-record-response test-record)
						       (test-record-query-string test-record)
						       parameters))))
