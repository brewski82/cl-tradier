(in-package #:cl-tradier)

(define-tradier-endpoints
  ;; Authentication
  (oauth/authorize :get (client-id "client_id") scope state)
  (oauth/accesstoken :post code (grant-type "grant_type"))
  ;; User Data
  (user/profile :get)
  (user/balances :get)
  (user/positions :get)
  (user/history :get)
  (user/gainloss :get)
  (user/orders :get)
  ;; Account Data
  (accounts/{account_id}/balances :get (account-id "account_id"))
  (accounts/{account_id}/positions :get (account-id "account_id"))
  (accounts/{account_id}/history :get (account-id "account_id") offset limit)
  (accounts/{account_id}/gainloss :get (account-id "account_id"))
  (accounts/{account_id}/orders :get (account-id "account_id"))
  (accounts/{account_id}/orders/{id} :get (account-id "account_id") id)
  ;; Trading
  (accounts/{account_id}/orders :post (account-id "account_id") class symbol duration
				side quantity type price stop (option-symbol "option_symbol") preview) ; TODO
  (accounts/{account_id}/orders/{id} :put (account-id "account_id") id type duration price stop)
  (accounts/{account_id}/orders/{id} :delete (account-id "account_id") id)
  ;; Market Data
  (markets/quotes :get symbols)
  (markets/timesales :get symbol interval start end (session-filter "session_filter"))
  (markets/options/chains :get symbol expiration)
  (markets/options/strikes :get symbol expiration)
  (markets/options/expirations :get symbol)
  (markets/history :get symbol interval start end)
  (markets/clock :get)
  (markets/calendar :get month year)
  (markets/search :get q indexes)
  (markets/lookup :get q exchanges types)
  (markets/events/session :post)
  ;; Fundamentals (Beta)
  (markets/fundamentals/company :get symbols)
  (markets/fundamentals/calendars :get symbols)
  (markets/fundamentals/dividends :get symbols)
  (markets/fundamentals/corporate_actions :get symbols)
  (markets/fundamentals/ratios :get symbols)
  (markets/fundamentals/financials :get symbols)
  (markets/fundamentals/statistics :get symbols)
  ;; Watchlists
  (watchlists :get)
  (watchlists/{id} :get id)
  (watchlists :post name symbols)
  (watchlists/{id} :put id name symbols)
  (watchlists/{id} :delete id)
  (watchlists/{id}/symbols :post id symbols)
  (watchlists/{id}/symbols/{symbol} :delete id symbol)
  ;; Streaming
  (markets/events :post sessionid symbols filter linebreak))
