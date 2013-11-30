(in-package #:check-test)

(defun ruby-scan-test ()
  (is (ruby-scan "h" "hoge") '("h") "ruby-scan 0")
  (is (ruby-scan "z" "hoge") nil "ruby-scan 1")
  (is (ruby-scan "[a-z]+" "hoge1234aeafea121aeafeaf")
      '("hoge" "aeafea" "aeafeaf") "ruby-scan 2")
  (is (ruby-scan "[0-9]+" "hoge1234aeafea121aeafeaf")
      '("1234" "121") "ruby-scan 3"))

(defparameter lineint '("9N006000000000001" "○" "体重" "体重" "数字" 1.0d0 "NNN.N" "PQ" "kg" "kg" 1.0d0
			0.0d0 NIL NIL NIL "9N006" "体重" NIL NIL "小数点以下1桁" NIL
			"1.2.392.200119.6.1005" NIL 20.0d0 250.0d0))
(defparameter linestr '("9N051000000000049" NIL "業務歴" "業務歴" "文字列" 2.0d0 256.0d0 "ST" NIL NIL
			1.0d0 0.0d0 NIL NIL NIL "9N051" "業務歴" NIL NIL "" NIL
			"1.2.392.200119.6.1005" NIL NIL ""))
(defparameter linecod '("9N056000000000011" "○" "既往歴" "既往歴" "コード" 2.0d0 "N" "CD" NIL NIL 1.0d0
			0.0d0 NIL NIL NIL "9N056" "既往歴" NIL NIL "1:特記すべきことあり､2:特記すべきことなし"
			"1.2.392.200119.6.2001" "1.2.392.200119.6.1005" NIL NIL ""))
