(defpackage :kensin/zenken
  (:nicknames :zenken :kz)
  (:use :cl :util :iterate :cl-win32ole :excel)
  (:import-from :cl-fad #:file-exists-p)
  (:export #:to-data
	   #:vec-output
	   #:calc
	   #:iterate
	   #:make-jusinken-hash
	   #:filter-map
	   #:shibu-list))
