(in-package #:cl-tradier)

(defmacro define-tradier-endpoint ((endpoint method) &body parameters)
  "Macro to define tradier api endpoints. ENDPOINT is the URL of the
endpoint sans the domain. METHOD is a http request
method: :get, :post, :put or :delete. Each parameter of the PARAMETERS
body is a symbol naming the path or query parameter or a list of the
form (symbol string) where symbol denotes a keyword agrument for the
method calling the api endpoint and string denotes the path or query
parameter for the http request."
  (let*
      ((parameter-alist (loop for parameter in parameters
			      collect (if (listp parameter)
					  (cons (first parameter) (second parameter))
					  (cons parameter (string-downcase parameter)))))
       (keys (mapcar #'first parameter-alist))
       (url-string (string-downcase endpoint))
       ;; Path parameters take the form {parameter} in the url string.
       (path-parameter-parts (cl-ppcre:all-matches-as-strings "{.+?}" url-string))
       (path-parameters (mapcar (lambda (parameter)
				  (cl-ppcre:regex-replace-all "[{}]" parameter ""))
				path-parameter-parts)))
    (with-gensyms (tradier-endpoint url method-var tradier-session)
      `(defmethod tradier-request ((,tradier-session tradier-session) (,tradier-endpoint (eql ',endpoint)) (,method-var (eql ',method)) &key ,@keys &allow-other-keys)
	 (let* ((,url (string-downcase ,tradier-endpoint)))
	   ,@(loop for parameter in path-parameters
		   for parameter-part in path-parameter-parts
		   for key = (car (rassoc parameter parameter-alist :test #'string-equal))
		   collect `(setf ,url (cl-ppcre:regex-replace-all ,parameter-part ,url (format nil "~A" ,key)))
		   do (setf parameter-alist (remove key parameter-alist :key #'car)))
	   (tradier-request ,tradier-session ,url ,method-var
			    :parameters (list ,@(loop for (symbol . string) in parameter-alist
						      collect `(cons ,string ,symbol)))))))))

(defmacro define-tradier-endpoints (&body body)
  "Convenience macro for defining multiple Tradier endpoints. BODY
denotes a list containing elements of the form (ENDPOINT METHOD
PARAMETERS)."
  `(progn
     ,@(loop for definition in body
	     for endpoint = (first definition)
	     for method = (second definition)
	     for parameters = (cddr definition)
	     collect `(define-tradier-endpoint (,endpoint ,method) ,@parameters))))
