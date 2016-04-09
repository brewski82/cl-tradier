(in-package #:cl-tradier)

;;; Authentication

(defun obtain-authorization-code (client-id scope state)
  (tradier-request *tradier-session* 'oauth/authorize :get :client-id client-id :scope scope :state state))

(defun obtain-access-token (code &optional (grant-type "authorization_code"))
  (tradier-request *tradier-session* 'oauth/accesstoken :post :code code :grant-type grant-type))

;;; User Data

(defun get-user-profile ()
  (tradier-request *tradier-session* 'user/profile :get))

(defun get-user-balances ()
  (tradier-request *tradier-session* 'user/balances :get))

(defun get-user-positions ()
  (tradier-request *tradier-session* 'user/positions :get))

(defun get-user-history ()
  (tradier-request *tradier-session* 'user/history :get))

(defun get-user-cost-basis ()
  (tradier-request *tradier-session* 'user/gainloss :get))

(defun get-user-orders ()
  (tradier-request *tradier-session* 'user/orders :get))

;;; Account Data

(defun get-account-balances (account-id)
  (tradier-request *tradier-session* 'accounts/{account_id}/balances :get :account-id account-id))

(defun get-account-positions (account-id)
  (tradier-request *tradier-session* 'accounts/{account_id}/positions :get :account-id account-id))

(defun get-account-history (account-id &key offset limit)
  (tradier-request *tradier-session* 'accounts/{account_id}/history :get :account-id account-id :offset offset :limit limit))

(defun get-account-cost-basis (account-id)
  (tradier-request *tradier-session* 'accounts/{account_id}/gainloss :get :account-id account-id))

(defun get-account-orders (account-id)
  (tradier-request *tradier-session* 'accounts/{account_id}/orders :get :account-id account-id))

(defun get-specifc-order-status (account-id id)
  (tradier-request *tradier-session* 'accounts/{account_id}/orders/{id} :get :account-id account-id :id id))

;;; Trading

(defun create-order (&key account-id class symbol duration side quantity type price stop option-symbol)
  (tradier-request *tradier-session* 'accounts/{account_id}/orders :post
		   :account-id account-id :class class :symbol symbol :duration duration :side side
		   :quantity quantity :type type :price price :stop stop :option-symbol option-symbol))

(defun create-multileg-order (&key account-id class symbol duration type price sides quantities option-symbols)
  "Creates a multileg order. Maps the lists SIDES, QUANTITIES and
OPTION-SYMBOLS to Tradier's side[index], quantity[index] and
option_symbol[index] parameters."
  (assert (and (every #'consp (list sides quantities option-symbols))
	       (apply #'= (mapcar #'length (list sides quantities option-symbols))))
	  (sides quantities option-symbols)
	  "SIDES, QUANTITIES and OPTION-SYMBOLS must be lists of the same length."
	  sides quantities option-symbols)
  (let ((extra-parameters (loop for i from 0
				for side in sides
				for quantity in quantities
				for option-symbol in option-symbols
				collect (cons (format nil "side[~A]" i) side)
				collect (cons (format nil "quantity[~A]" i) (format nil "~A" quantity))
				collect (cons (format nil "option_symbol[~A]" i) option-symbol))))
    (tradier-request *tradier-session* 'accounts/{account_id}/orders :post
		     :account-id account-id :class class :symbol symbol :duration duration :type type :price price
		     :extra-parameters extra-parameters)))

(defun preview-order (&key account-id preview class symbol duration side quantity type price stop option-symbol)
  (tradier-request *tradier-session* 'accounts/{account_id}/orders :post
		   :account-id account-id :preview preview :class class :symbol symbol :duration duration
		   :side side :quantity quantity :type type :price price :stop stop
		   :option-symbol option-symbol))

(defun change-order (account-id id &key type duration price stop)
  (tradier-request *tradier-session* 'accounts/{account_id}/orders/{id} :put
		   :account-id account-id :id id :type type :duration duration
		   :price price :stop stop))

(defun cancel-order (account-id id)
  (tradier-request *tradier-session* 'accounts/{account_id}/orders/{id} :delete :account-id account-id :id id))

;;; Market Data

(defun get-quotes (&rest symbols)
  (tradier-request *tradier-session* 'markets/quotes :get :symbols (comma-separated-string symbols)))

(defun get-option-quote (symbol strike-price date type)
  "Convenience function to get a single option quote."
  (get-quotes (make-option-symbol symbol strike-price date type)))

(defun get-time-and-sales (symbol &rest keys &key interval start end session-filter)
  (declare (ignorable interval start end session-filter))
  (apply #'tradier-request *tradier-session* 'markets/timesales :get :symbol (string symbol) keys))

(defun get-option-chain (symbol expiration)
  (tradier-request *tradier-session* 'markets/options/chains :get :symbol (string symbol) :expiration expiration))

(defun get-option-strikes (symbol expiration)
  (tradier-request *tradier-session* 'markets/options/strikes :get :symbol (string symbol) :expiration expiration))

(defun get-option-expirations (symbol)
  (tradier-request *tradier-session* 'markets/options/expirations :get :symbol (string symbol)))

(defun get-historical-pricing (symbol &key interval start end)
  (tradier-request *tradier-session* 'markets/history :get :symbol (string symbol) :interval interval :start start :end end))

(defun get-intraday-status ()
  (tradier-request *tradier-session* 'markets/clock :get))

(defun get-market-calendar (&key month year)
  (tradier-request *tradier-session* 'markets/calendar :get :month month :year year))

(defun search-for-company (q &optional indexes)
  (tradier-request *tradier-session* 'markets/search :get :q q :indexes indexes))

(defun lookup-symbol (&key q exchanges types)
  (tradier-request *tradier-session* 'markets/lookup :get :q q :exchanges exchanges :types types))

(defun create-streaming-session ()
  (tradier-request *tradier-session* 'markets/events/session :post))

;;; Fundamentals (Beta)

(defun get-company-information (&rest symbols)
  (tradier-request *tradier-session* 'markets/fundamentals/company :get :symbols (comma-separated-string symbols)))

(defun get-company-information-beta (&rest symbols)
  (with-tradier-context (:endpoint-version +beta-version+)
    (apply #'get-company-information symbols)))

(defun get-corporate-calendars (&rest symbols)
  (tradier-request *tradier-session* 'markets/fundamentals/calendars :get :symbols (comma-separated-string symbols)))

(defun get-corporate-calendars-beta (&rest symbols)
  (with-tradier-context (:endpoint-version +beta-version+)
    (apply #'get-corporate-calendars symbols)))

(defun get-dividend-information (&rest symbols)
  (tradier-request *tradier-session* 'markets/fundamentals/dividends :get :symbols (comma-separated-string symbols)))

(defun get-dividend-information-beta (&rest symbols)
  (with-tradier-context (:endpoint-version +beta-version+)
    (apply #'get-dividend-information symbols)))

(defun get-corporate-actions (&rest symbols)
  (tradier-request *tradier-session* 'markets/fundamentals/corporate_actions :get :symbols (comma-separated-string symbols)))

(defun get-corporate-actions-beta (&rest symbols)
  (with-tradier-context (:endpoint-version +beta-version+)
    (apply #'get-corporate-actions symbols)))

(defun get-operation-ratios (&rest symbols)
  (tradier-request *tradier-session* 'markets/fundamentals/ratios :get :symbols (comma-separated-string symbols)))

(defun get-operation-ratios-beta (&rest symbols)
  (with-tradier-context (:endpoint-version +beta-version+)
    (apply #'get-operation-ratios symbols)))

(defun get-financial-reports (&rest symbols)
  (tradier-request *tradier-session* 'markets/fundamentals/financials :get :symbols (comma-separated-string symbols)))

(defun get-financial-reports-beta (&rest symbols)
  (with-tradier-context (:endpoint-version +beta-version+)
    (apply #'get-financial-reports symbols)))

(defun get-price-statistics (&rest symbols)
  (tradier-request *tradier-session* 'markets/fundamentals/statistics :get :symbols (comma-separated-string symbols)))

(defun get-price-statistics-beta (&rest symbols)
  (with-tradier-context (:endpoint-version +beta-version+)
    (apply #'get-price-statistics symbols)))

;;; Watchlists

(defun get-all-watchlists ()
  (tradier-request *tradier-session* 'watchlists :get))

(defun get-watchlist (id)
  (tradier-request *tradier-session* 'watchlists/{id} :get :id id))

(defun create-watchlist (name symbols)
  (tradier-request *tradier-session* 'watchlists :post :name name :symbols (comma-separated-string symbols)))

(defun update-watchlist (id name symbols)
  ;; TODO: Confirm if SYMBOLS is indeed required as stated by the spec.
  (tradier-request *tradier-session* 'watchlists/{id} :put :id id :name name :symbols (comma-separated-string symbols)))

(defun delete-watchlist (id)
  (tradier-request *tradier-session* 'watchlists/{id} :delete :id id))

(defun add-symbols (id symbols)
  (tradier-request *tradier-session* 'watchlists/{id}/symbols :post :id id :symbols (comma-separated-string symbols)))

(defun remove-symbol (id symbol)
  (tradier-request *tradier-session* 'watchlists/{id}/symbols/{symbol} :delete :id id :symbol symbol))

;;; Streaming

(defun stream-quotes (session-id symbols &key filter linebreak (want-stream t))
  (with-tradier-context (:request-fn (lambda (url &rest args)
				       (apply #'drakma:http-request url :want-stream want-stream args))
			 :environment :streaming)
    (tradier-request *tradier-session* 'markets/events :post
		     :sessionid session-id
		     :symbols (comma-separated-string symbols)
		     :filter (when filter (comma-separated-string filter))
		     :linebreak linebreak)))
