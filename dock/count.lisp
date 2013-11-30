(in-package :dock)

(defun shibu-select (list)
  (iter (for l :in list)
	(chash l :key #'dock-支部)))

(defun count-up (year)
  (iter (for name :in (namelist :hash-table (make-hash-table)))
	(for old = (how-old (dock-生年月日 name)
			    (nendo-end (read-from-string ks::year))))
	(with-dock-slot name
	  (if (and (<= 40 old) (>= 75 old) 受診日
		   (not 脳単独) (eq 年度 (read-from-string year)))
	      (collect name :into pot)))
	(finally (return (shibu-select pot)))))

(defun count-printer (hash shibu-print)
  (iter (for (k v) :in-hashtable hash)
	(if shibu-print
	    (format *standard-output*
		    "~{~A~^,~}~%" (list k v))
	    (format *standard-output* "~A~%" v))))

(in-package :cl-user)
