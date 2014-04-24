(defpackage :kensin/hsido
  (:nicknames :hsido :hs)
  (:use :cl :util :iterate :optima)
  (:import-from #:cl-test-more #:is)
  (:import-from #:cl-win32ole
		#:ole)
  (:import-from #:excel
		#:with-excel
		#:with-excel-book)
  (:export #:@write))

(defpackage :hsido-fkbb331
  (:nicknames #:hfk)
  (:use :cl :util :kensin :iterate :optima)
  (:export :make-csv))
