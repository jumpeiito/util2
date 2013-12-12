(defpackage :kensin/zipmove
  (:nicknames :kzm)
  (:use :cl :util :iterate :sb-thread)
  (:export #:move)
  (:import-from #:cl-fad
		#:file-exists-p))
