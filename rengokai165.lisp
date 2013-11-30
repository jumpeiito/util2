(in-package :kensin)

(eval-when (:compile-toplevel)
  (cl-user::rename-package :cl-irregsexp :cl-irregsexp '(:irr)))

;; "11118058201"

;; (defparameter 2011zip #P"f:/zip/FKAC165/00263129_FKAC165_20121015214055_1.zip")
;; (defparameter 2010zip #P"f:/zip/FKAC165/00263129_FKAC165_20110927214232_1.zip")
;; (defparameter 2009zip #P"f:/zip/FKAC165/00263129_FKAC165_20101108215428_1.zip")

;; (defparameter 2009csv (make-pathname :defaults 2009zip :type "csv"))
;; (defparameter 2010csv (make-pathname :defaults 2010zip :type "csv"))
;; (defparameter 2011csv (make-pathname :defaults 2011zip :type "csv"))

(defparameter topdir (or ;;(cl-fad:file-exists-p #P"D:/zip/")
			 (cl-fad:file-exists-p #P"F:/zip/")))

(defparameter directories
  (mapcar (lambda (path) (util::path+ topdir path))
	  '("/FKAC165/" "/ke26312901/")))

(defun jnum->rnum (jnum)
  (irr:if-match-bind ((year (string 2)) (string) (subst (string 8)))
		     (the string jnum)
		     (format nil "~A1~A" year subst)
		     jnum))

(defun r165? (pathname)
  (let1 name (if (pathnamep pathname)
		 (pathname-name pathname)
		 (irr:if-match-bind (_ "/") (the string pathname)
				    (second (irr:match-split "/" (the string pathname)))
				    pathname))
    (irr:if-match-bind ("ke26312901" _) (the string name)
		       pathname
		       (irr:if-match-bind (_ "FKAC165") (the string name)
		       			  pathname
		       			  nil))))

(defun zip-to-filelist (zipfile)
  (remove-if-not
   #'r165?
   (mapcar #'car (util::zipfile-entry zipfile))))

(defun r165-list ()
  (iter (for dir :in directories)
	(appending
	 (iter (for zipfile :in-directory dir :type "zip")
	       (if (zip-to-filelist zipfile)
		   (collect zipfile))))))

(defun ren (&rest args)
  (print (list args)))

(defun r165-unzip ()
  (iter (for zip :in (r165-list))
	(funcall #+sbcl  #'sb-ext:run-program
		 #+clisp #'run-program
	 "f:/UnxUtils/usr/local/wbin/unzip.exe"
	 `(,(format nil "~A" zip)
	    "-d"
	    ,(format nil "~A" (make-pathname :defaults zip
					     :name nil
					     :type nil)))
	 :wait t)))

(defun get-contents-base (entry)
  (& #+sbcl sb-ext:octets-to-string #+clisp babel:octets-to-string
     zip:zipfile-entry-contents
     entry))

(defun get-contents (zipfile)
  (zip:with-zipfile (z zipfile :external-format :SJIS)
    (iter (for (k v) :in-hashtable (zip:zipfile-entries z))
	  (if (r165? k)
	      (print (get-contents-base v))))))

(defstruct r165
  記号 番号 生年月日 性別 年度 階層化 利用券番号 実施日 医療機関コード
  レベル 初回日 中間日 評価日 医療機関 受診券番号)

(defun create-instance (line)
  (let1 obj (make-r165)
    (with-slots (記号 番号 生年月日 性別 年度 階層化 利用券番号 実施日
		      医療機関 レベル 初回日 中間日 評価日 医療機関コード
		      受診券番号) obj
      (r167-setq 記号		1
		 番号		2
		 生年月日	(4 #'r167-date)
		 性別		5
		 年度		8
		 階層化		9
		 利用券番号	10
		 受診券番号	100
		 実施日		(12 #'r167-date)
		 医療機関コード	13
		 医療機関	(13 (lambda (o) (car (gethash o hospital-hash))))
		 レベル		14
		 初回日		(17 #'r167-date)
		 中間日		(53 #'r167-date)
		 評価日		(68 #'r167-date)))
    obj))

(defun initial (csvfile)
  (iter (for line :in-csv csvfile :code :SJIS)
	(for row :upfrom 1)
	(if (> row 2)
	    (collect (create-instance line)))))

(defun 165jhash (csvfile)
  (iter (for instance :in (initial csvfile))
	(shash instance
	       :condition t
	       :key #'r165-受診券番号)))

(defun 165rhash (csvfile)
  (iter (for instance :in (initial csvfile))
	(shash instance
	       :condition t
	       :key #'r165-利用券番号)))

;; d:/zip/ke26312901/ke26312901_20130701093131.zip


(in-package :cl-user)
