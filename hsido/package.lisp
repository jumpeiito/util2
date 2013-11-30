(defpackage :kensin/hsido
  (:nicknames :hsido :hs)
  (:use :cl :util :iterate)
  (:export #:output
	   #:collect
	   #:repair-jnumber
	   #:topdir))
