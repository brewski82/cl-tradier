(in-package #:cl-tradier)

;;; Constants.

(define-constant +sandbox-domain+ "https://sandbox.tradier.com" :test #'string-equal)
(define-constant +request-domain+ "https://api.tradier.com" :test #'string-equal)
(define-constant +streaming-domain+ "https://stream.tradier.com" :test #'string-equal)
(define-constant +beta-version+ "beta" :test #'string-equal)
(define-constant +version-1+ "v1" :test #'string-equal)

;;; Specials.

(defparameter *tradier-environment* nil
  "One of :prod or :sandbox.")
(defparameter *tradier-endpoint-version* nil
  "One of v1 or beta.")
(defparameter *tradier-session* nil
  "Holds the current session object of class TRADIER-SESSION.")
(defparameter *tradier-response-format* nil
  "One of :xml, :json, :javascript. Will default to :json.")
(defparameter *tradier-request-fn* #'drakma:http-request
  "Function for making HTTP calls. Defaults to the drakma:http-request")
(defparameter *tradier-text-content-types* (list (cons "text" nil) (cons nil "text") (cons nil "json") (cons nil "xml"))
  "Responses matching these content types will automatically convert
to strings. See drakma:*text-content-types* for more information.")

;;; Tradier session initialization.

(defclass tradier-session ()
  ((access-token :initarg :access-token :initform nil :accessor access-token))
  (:documentation "Holds information specific to the current Tradier
session. Subclass to override default behavior of TRADIER-REQUEST and
related generic functions."))

(defun set-tradier-endpoint-version (version)
  (setf *tradier-endpoint-version* version))

(defun set-tradier-environment (environment)
  (unless (member environment (list :prod :sandbox))
    (error 'tradier-initialization-error :message (format nil "Unrecognized environment: ~A. Valid environments: :prod, :sandbox." environment)))
  (setf *tradier-environment* environment))

(defun set-response-format (format)
  (unless (member format (list :xml :json :javascript))
    (error 'tradier-initialization-error :message (format nil "Unrecognized response format: ~A. Valid formats: :xml, :json, :javascript." format)))
  (setf *tradier-response-format* format))

(defun initialize-tradier-session (&key environment (endpoint-version +version-1+) (default-response-format :json) access-token (tradier-session-class 'tradier-session))
  (set-tradier-endpoint-version (string-downcase endpoint-version))
  (set-tradier-environment environment)
  (set-response-format default-response-format)
  (setf *tradier-session* (make-instance tradier-session-class :access-token access-token)))

;;; Helper utils.

(defmacro with-tradier-context ((&key environment endpoint-version response-format request-fn) &body body)
  `(let ((*tradier-environment* (or ,environment *tradier-environment*))
	 (*tradier-endpoint-version* (or ,endpoint-version *tradier-endpoint-version*))
	 (*tradier-response-format* (or ,response-format *tradier-response-format*))
	 (*tradier-request-fn* (or ,request-fn *tradier-request-fn*)))
     (progn ,@body)))

(defun make-option-symbol (symbol strike-price date type)
  "Helper function to construct an option symbol. Date must have the
form yyyy-mm-dd, the same format used by Tradier."
  (format nil "~A~A~A~A~A~8,'0d"
	  symbol
	  (subseq date 2 4)
	  (subseq date 5 7)
	  (subseq date 8 10)
	  type
	  (round (* 1000 strike-price))))

(defun comma-separated-string (list)
  "Accepts a list of objects and returns them as a comma separated
string."
  (format nil "~{~A~^,~}" (ensure-list list)))
