(in-package :kensin)

(defparameter 331dir #P"d:/特定健診システム/特定健診利用券情報/FKBB331.CSV")

(defun 331hash (sym)
  (iter (with func = (case sym
		       (:id   #'fifth)
		       (:jnum #'tenth)
		       (:rnum #'ninth)
		       (t     #'tenth)))
	(for line :in-csv 331dir :code :SJIS)
	(if (> (length line) 10)
	    (shash line
		   :condition t
		   :key func))))

(defun 331-header (op)
  (cl-irregsexp:if-match-bind
   ((string 8) (time (string 6)))
   (the string (util::now-string))
   (format op "~{~A~^,~}~%"
	   `("00263129" "FKBB331" ,(util::today-8) ,time))))

(defun 331-contents (list op)
  (iter (with hash = (331hash :rnum))
	(for num :in list)
	(format op "~{~A~^,~}~%" (gethash num hash nil))
	(finally (format op "~A~%" (length list)))))

(defun make-331-file (list)
  (call-with-output-file2 "FKBB331.csv"
    (lambda (o)
      (331-header o)
      (331-contents list o))
    :code :SJIS))

(defun zipfile-to-331data (zipfile)
  (iter (with hash = (331hash :rnum))
	(for person :in (kensin::hs/parse-zip-main zipfile))
	(for rnumber = (third person))
	;; (format *standard-output* "~{~A~^,~}~%" (gethash rnumber hash))
	(collect (gethash rnumber hash))))

(mapcar
 (lambda (b)
   (cons b (zipfile-to-331data b)))
 (directory-list #P"g:/20130823_up/" :type "zip"))

(in-package :cl-user)
