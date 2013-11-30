(defpackage :kensin/csv
  (:nicknames :kcsv)
  (:use :cl :util :iterate :cl-win32ole :excel)
  (:import-from #:cl-irregsexp
  		#:if-match-bind)
  )
