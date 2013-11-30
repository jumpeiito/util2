(asdf:defsystem :kensin
  :version "0"
  :depends-on (:util :iterate :cl-fad :cl-irregsexp :bordeaux-threads :cl-test-more :cl-who :optima
		     :cl-match :cl-store)
  :components
  ((:file "package")
   (:module :ksetting
   	    :components ((:file "package")
   			 (:file "setting" :depends-on ("package"))
			 ;; (:file "setter"  :depends_on ("package"))
			 ))
   (:file "kensin-xml"	:depends-on ("package"))
   (:module :zenken
   	    :components ((:file "package")
   			 (:file "zenken" :depends-on ("package"))))
   (:file "kensinb"	:depends-on ("package"))
   (:file "rengokai167"	:depends-on ("package"))
   ;; (:file "rengokai165"	:depends-on ("package"))
   (:file "rengokai331"	:depends-on ("package"))
   (:file "shibu"	:depends-on ("package"))
   (:module :uplog
   	    :components ((:file "package")
   			 (:file "uplog"  :depends-on ("package"))))

   ;; (:module :hsido
   ;; 	    :components ((:file "package")
   ;; 			 (:file "hsido" :depends-on ("package"))))
   (:module :csv
   	    :components ((:file "package")
   			 (:file "code"    :depends-on ("package"))
   			 (:file "csv"     :depends-on ("package" "code"))
   			 (:file "counter" :depends-on ("package" "csv"))
   			 (:file "duplicate" :depends-on ("package" "csv"))
   			 ;; (:file "check-error"  :depends-on ("package"))
   			 ))
   (:module :csv-error
   	    :components ((:file "package")
   			 (:file "csv-error" :depends-on ("package"))))
   #-clisp
   (:module :zipmove
   	    :components ((:file "package")
   			 (:file "zipmove" :depends-on ("package"))))
   #-clisp
   (:module :zipget
   	    :components ((:file "package")
   			 (:file "zipget" :depends-on ("package"))))
   (:module :xmlrep
   	    :components ((:file "package")
   			 (:file "xmlrep" :depends-on ("package"))))
   (:module :kxml
   	    :components ((:file "package")
   			 (:file "kxml" :depends-on ("package"))
   			 (:file "kxml-print"
   				:depends-on ("package" "kxml"))))
   (:module :dock
   	    :components ((:file "package")
   			 (:file "dock"  :depends-on ("package"))
   			 (:file "count" :depends-on ("package" "dock"))
   			 (:file "alert" :depends-on ("package" "dock"))))
   (:module :receive
   	    :components ((:file "package")
   			 (:file "receive" :depends-on ("package"))))
   (:module :kserv
   	    :components ((:file "package")
   			 (:file "kserv" :depends-on ("package"))))
   (:module :hsidoxml
   	    :components ((:file "package")
   			 (:file "code")
   			 (:file "hsidoxml" :depends-on ("package" "code"))))
   (:module :xls-value-check
   	    :components ((:file "package")
   			 (:file "xvc" :depends-on ("package"))))
   (:module :upload
	    :components ((:file "package")
   			 (:file "upload" :depends-on ("package"))))
   (:file "rengokai172"	:depends-on ("package" "rengokai167"))
   (:module :meibo
	    :components ((:file "meibo")))
   (:module :xls-checker
	    :components ((:file "package")
			 (:file "xls-checker" :depends-on ("package"))
			 (:file "xls-checker-test" :depends-on ("package"))))))
