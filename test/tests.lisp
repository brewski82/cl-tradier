(in-package #:cl-tradier-test)

(def-suite :cl-tradier-test-suite :description "cl-traider functional tests.")

(in-suite :cl-tradier-test-suite)

(defun alist-to-url-string (alist)
  "Util method to convert alist parameters to a query string for
comparison."
  (with-output-to-string (out)
    (loop for first = t then nil
	  for (name . value) in alist
	  unless first do (write-char #\& out)
	  do (format out "~A=~A" name value))))

(defun run-tests-in-test-records ()
  "Loops through each record in *TEST-RECORDS* and performs the
test."
  (loop for test-record in *test-records* do
    (multiple-value-bind (response query-string parameter-alist)
	(apply (test-record-test-function test-record)
	       (test-record-test-paramaters test-record))
      (is (string-equal (test-record-response test-record)
			response))
      (when parameter-alist
	(is (string-equal query-string (alist-to-url-string parameter-alist)))))))

(test api-tests
  "Call each Tradier API function and ensure we get the expected results."
  (run-tests-in-test-records))

(test multileg-order
  (let ((*test-records* (list (make-test-record :test-function #'create-multileg-order
						:test-paramaters '(:account-id 12345678 :class "multileg" :symbol "CSCO"
								   :duration "day" :type "market" :sides ("buy_to_open" "sell_to_open")
								   :quantities (1 1)
								   :option-symbols ("CSCO150117C00035000" "CSCO140118C00008000"))
						:url "https://api.tradier.com/v1/accounts/12345678/orders"
						:method :post
						:query-string "class=multileg&symbol=CSCO&duration=day&type=market&side[0]=buy_to_open&quantity[0]=1&option_symbol[0]=CSCO150117C00035000&side[1]=sell_to_open&quantity[1]=1&option_symbol[1]=CSCO140118C00008000"
						:response "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<order>
    <id>2098</id>
    <status>ok</status>
</order>"))))
    (run-tests-in-test-records)))

(test preview-order
  (let ((*test-records* (list (make-test-record :test-function #'preview-order
						:test-paramaters '(:account-id 12345678 :symbol "AAPL" :duration :day
								   :side "buy" :quantity 100 :type "market"
								   :preview "true")
						:url "https://api.tradier.com/v1/accounts/12345678/orders"
						:method :post
						:query-string "symbol=AAPL&duration=day&side=buy&quantity=100&type=market&preview=true"
						:response "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<order>
    <commission>3.490000</commission>
    <cost>1917.090000</cost>
    <extended_hours>false</extended_hours>
    <fees>0.000000</fees>
    <margin_change>956.6500000</margin_change>
    <quantity>10.000000</quantity>
    <status>ok</status>
</order>"))))
    (run-tests-in-test-records)))
