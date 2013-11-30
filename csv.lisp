(in-package :kensin)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cl-irregsexp))

(defstruct csv iscode isname hcode hname cday oday index
	   body length)

(defstruct csv-line name rownum)

(in-package :cl-user)
