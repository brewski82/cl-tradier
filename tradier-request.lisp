(in-package #:cl-tradier)

(defgeneric tradier-request (tradier-session tradier-endpoint method &key &allow-other-keys)
  (:documentation "Base function for executing http api calls to
Tradier. Each Tradier endpoint should have its own specialized
method. Calls TRADIER-HTTP-REQUEST with the Tradier url and other
keyword parameters. Passes the returned values as a list to
PROCESS-TRADIER-RESPONSE and returns its values."))

(defgeneric tradier-http-request (tradier-session url &key &allow-other-keys)
  (:documentation "Performs the actual http Tradier API request. Uses
the function bound by *TRADIER-REQUEST-FN*, bound to
DRAKMA:HTTP-REQUEST by default."))

(defgeneric process-tradier-response (tradier-session response-values &key &allow-other-keys)
  (:documentation "Performs any additional processing to the Tradier
response before returning the response to the caller. Useful method to
specialize for decoding xml or json into lisp objects."))

(defgeneric check-response-for-errors (tradier-session response-values &key &allow-other-keys)
  (:documentation "Accepts the values returned by TRADIER-HTTP-REQUEST
as a list and checks them for errors."))

(defun access-token-header ()
  (when *tradier-session*
    (list (cons "Authorization" (concatenate 'string "Bearer " (access-token *tradier-session*))))))

(defun accept-header ()
  (ecase *tradier-response-format*
    (:xml "application/xml")
    (:json "application/json")
    (:javascript "application/javascript")))

(defparameter *extra-parameters* nil
  "When non-nil, should hold additional query paramters to include in
the request.")

(defun construct-url (url)
  (unless *tradier-environment* (error 'tradier-initialization-error :message "Please specify the tradier api environment via INITIALIZE-TRADIER-SESSION"))
  (unless *tradier-endpoint-version* (error 'tradier-initialization-error :message "Please specify the tradier api version via INITIALIZE-TRADIER-SESSION "))
  (let ((domain (ecase *tradier-environment*
		  (:prod +request-domain+)
		  (:sandbox +sandbox-domain+)
		  (:streaming +streaming-domain+))))
    (concatenate 'string domain "/" *tradier-endpoint-version* "/" url)))

(defmethod tradier-http-request ((tradier-session tradier-session) (url string) &key additional-headers method parameters accept &allow-other-keys)
  (let ((drakma:*text-content-types* *tradier-text-content-types*))
    (funcall *tradier-request-fn* url :additional-headers additional-headers :method method :parameters parameters :accept accept)))

(defmethod check-response-for-errors ((tradier-session tradier-session) response-values &key &allow-other-keys)
  "Default error checking for DRAKMA:HTTP-REQUEST results."
  (when (= 7 (length response-values))
    (destructuring-bind (body status-code headers uri stream close-stream-p reason)
	response-values
      (when (string-equal "Quota Violoation" reason)
	(error 'tradier-quota-error :http-status-code status-code
				    :response-body body
				    :headers headers
				    :uri uri
				    :response-stream stream
				    :close-stream-p close-stream-p
				    :reason reason
				    :rate-limit-allowed (assoc-value headers :x-ratelimit-allowed)
				    :rate-limit-available (assoc-value headers :x-ratelimit-available)
				    :rate-limit-used (assoc-value headers :x-ratelimit-used)
				    :rate-limit-expiry (assoc-value headers :x-ratelimit-expiry)))
      (when (<= 400 status-code 599)
	(error 'tradier-response-error :http-status-code status-code
				       :response-body body
				       :headers headers
				       :uri uri
				       :response-stream stream
				       :close-stream-p close-stream-p
				       :reason reason)))))

(defmethod process-tradier-response ((tradier-session tradier-session) response-values &key &allow-other-keys)
  (check-response-for-errors tradier-session response-values)
  (values-list response-values))

(defmethod tradier-request ((tradier-session tradier-session) (tradier-endpoint string) method &key parameters &allow-other-keys)
  "Base method for Tradier API calls. Returns the multiple values
returned from DRAKMA:HTTP-REQUEST."
  (let* ((full-parameters (remove-if #'null
				     (append parameters *extra-parameters*)
				     :key #'cdr))
	 (response-values (multiple-value-list (tradier-http-request tradier-session
								     (construct-url tradier-endpoint)
								     :additional-headers (access-token-header)
								     :method method
								     :parameters full-parameters
								     :accept (accept-header)))))
    (process-tradier-response tradier-session response-values)))

(defmethod tradier-request :around ((tradier-session tradier-session) tradier-endpoint method &key extra-parameters &allow-other-keys)
  (let ((*extra-parameters* (append *extra-parameters* extra-parameters)))
    (call-next-method)))
