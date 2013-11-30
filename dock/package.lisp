(defpackage :kensin/dock
  (:nicknames :kd :dock)
  (:import-from #:cl-fad
		#:file-exists-p
		#:walk-directory)
  (:use :cl :util :iterate :cl-win32ole :excel))
