(defpackage :kensin/serv
  (:nicknames :kserv)
  (:use :cl :util :iterate :cl-win32ole :excel)
  (:import-from #:cl-fad
		#:copy-file
		#:file-exists-p
		#:pathname-as-directory))
