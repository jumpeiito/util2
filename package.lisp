(defpackage :kensin
  (:nicknames :kensin)
  (:use :cl :util :iterate :cl-win32ole :excel)
  (:import-from #:iterate
  		#:iter
  		#:for
  		#:repeat
  		#:collect
  		#:counting
  		#:maximize
  		#:generating
  		#:next
  		#:sum
  		#:with
  		#:finally
  		#:appending
  		#:first-time-p
  		#:defmacro-clause
  		#:defmacro-driver)
  (:import-from #:cl-ppcre
  		#:scan
  		#:regex-replace-all
  		#:scan-to-strings
  		#:split)
  #+sbcl (:import-from #:sb-impl #:fd-stream-file)
  (:import-from #:cxml-stp #:make-builder)
  (:import-from #:cl-fad #:file-exists-p)
  (:export #:kx/parse
  	   #:kx/parse2
  	   #:hs/parse-zip-main
  	   #:rengokai-file-p
  	   #:hospital-hash
  	   #:hospital-short-hash
  	   #:dock-hash
  	   #:dock?
  	   #:hospital-shibu-hash
  	   #:shibu-kenshin?
  	   #:code->hospital
  	   #:kgbg-regexp
	   #:172-newest
  	   #:172data
  	   #:172-hash
  	   #:172-complex-hash
  	   #:172-hash2
  	   #:short-vice-shibu-hash
  	   #:long-vice-shibu-hash
  	   #:short-shibu
  	   #:long-shibu
  	   #:kgbg
  	   #:shibu
  	   #:shibu>
  	   #:shibu<
  	   #:bunkai-hash
  	   #:r167-hash
  	   #:167-total
	   #:kensin-year?
	   #:jnum-how-old
	   #:sex
	   #:jnum->sex
	   #:hk
	   #:r165-iterate
	   #:r165-hash))

(defpackage #:R172-core
  (:nicknames #:R172c)
  (:use :cl :util :kensin :iterate)
  )
