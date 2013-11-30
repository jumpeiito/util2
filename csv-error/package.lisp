(defpackage :kensin/csv-error
  (:nicknames :cerr)
  (:use :cl :util :iterate :cl-win32ole :excel)
  (:import-from #:cl-irregsexp
  		#:if-match-bind))
