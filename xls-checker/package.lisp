(defpackage #:xls-checker
  (:nicknames :check)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:import-from #:optima #:match)
  (:import-from #:cl-irregsexp #:if-match-bind)
  (:export #:sheet
	   #:reload!
	   #:sheet-contents
	   #:fcolor
	   #:rcolor
	   #:value!
	   #:code->index
	   #:num->col
	   #:col->num))

(defpackage #:xls-checker-test
  (:nicknames :check-test :cht)
  (:use :cl :util :kensin :iterate)
  (:import-from #:cl-test-more
		#:is))

(defpackage #:xls-checker-code
  (:nicknames :check-code :chc)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:import-from #:optima #:match)
  (:export #:Code
	   #:CodeString
	   #:CodeNumber
	   #:CodeCode
	   #:info))

(defpackage #:xls-checker-formatta
  (:nicknames :check-formatta :chf)
  (:use :cl :util :kensin)
  (:import-from #:cl-ppcre
		#:register-groups-bind
		#:scan)
  (:export #:match))

(defpackage #:xls-checker-repair-value
  (:nicknames :check-repval :chrv)
  (:use :cl :util :kensin)
  (:import-from #:cl-ppcre
		#:regex-replace-all)
  (:export #:repair
	   #:codenumber-out-of-order
	   #:out-of-selection))

(defpackage #:xls-checker-body
  (:nicknames :check-body :chbody)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:repair))

(defpackage #:xls-checker-base
  (:nicknames :check-base :chbase)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:import-from #:cl-ppcre
		#:scan
		#:register-groups-bind)
  (:export #:name-repair
	   #:birthday-repair
	   #:insurance-repair))

(defpackage #:xls-checker-hba1c
  (:nicknames :check-hba1c :chb)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:repair))

(defpackage #:xls-checker-relative
  (:nicknames :check-relative :chrel)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:repair))

(defpackage #:xls-checker-must
  (:nicknames :check-must :chm)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:repair))

(defpackage #:xls-checker-available
  (:nicknames :check-available :chavl)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:repair))

(defpackage #:xls-checker-either
  (:nicknames :check-either :cheth)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:repair))

(defpackage #:xls-checker-hanzen
  (:nicknames :check-hanzen :chz)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:repair))
