(defpackage #:meibo
  (:nicknames :mb)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel))

(in-package :mb)

(defvar *year*		2013)
(defvar *nendo-end*	(nendo-end *year*))
(defvar *zenken-hash*	"f:/util2/meibo/zenken.id.hash")

(defclass MeiboCSV ()
  ((pathname	:initarg :pathname
		:accessor path->)
   (shibu	:accessor shibu->)
   (shibu-code	:accessor shibu-c->)
   (body	:accessor body->)))

(defclass MeiboLine ()
  ((list	:initarg :list :accessor list->)
   (jnumber	:accessor jnumber->)
   (name	:accessor name->)
   (furigana	:accessor furigana->)
   (gender	:accessor gender->)
   (birthday	:accessor birthday->)
   (old		:accessor old->)
   (kigo	:accessor kigo->)
   (bango	:accessor bango->)
   (hk		:accessor hk->)
   (telnum	:accessor telnum->)
   (knumber	:accessor knumber->)
   (idnumber	:accessor idnumber->)))

;; #P"f:/util2/meibo/特定健診用氏名一覧19西_20131127.csv"
(defun %pathname-parse (pathname)
  (let ((name (pathname-name (make-pathname :defaults pathname))))
    (cl-irregsexp:if-match-bind
     ("特定健診用氏名一覧" (code (integer :length 2)) (shibu (string)) "_" _)
     (the simple-string name)
     (values code shibu))))

(defun %get-body (pathname)
  (iter (for line :in-csv pathname :code :SJIS)
	(optima:match line
	  ((LIST* "受診券整理番号" _)
	   (next-iteration))
	  ((LIST* _ _ _ _ birth _)
	   (if (kensin::kensin-year?
		(how-old birth *nendo-end*))
	       (collect line))))))

(defun %copy-zenken-file ()
  (delete-file-if-exists
   "f:/20130628/特定健診全件データ.csv")
  (cl-fad:copy-file "d:/特定健診システム/特定健診CSV/特定健診全件データ.csv"
		    "f:/20130628/特定健診全件データ.csv"))

(defun %make-zenken-hash ()
  (%copy-zenken-file)
  (cl-store:store (zenken::id-hash "f:/20130628/特定健診全件データ.csv" *nendo-end*)
		  *zenken-hash*))

(defmethod initialize-instance :after ((m MeiboCSV) &rest args)
  (declare (ignorable args))
  (with-slots (pathname) m
    (multiple-value-bind (code shibu)
	(%pathname-parse pathname)
      (setf (shibu-> m)		shibu
	    (shibu-c-> m)	code
	    (body-> m)		(%get-body pathname)))))

(defmethod initialize-instance :after ((m MeiboLINE) &rest args)
  (declare (ignorable args))
  (with-slots (jnumber name furigana gender birthday old
		       kigo bango hk telnum knumber idnumber) m
    (optima:match (list-> m)
      ((LIST* j n f g b o ki bg h _ _ tel kn _ id _)
       (setq jnumber	j
       	     name	n
       	     furigana	f
       	     gender	g
       	     birthday	b
       	     old	o
       	     kigo	ki
       	     bango	bg
       	     hk		h
       	     telnum	tel
       	     knumber	kn
       	     idnumber	id))
      (_ (print (list-> m))))))

(defun figure (line)
  (declare (type MeiboLINE line))
  (with-slots (kigo bango hk name furigana gender birthday old telnum) line
    (list kigo bango hk name furigana gender birthday old telnum)))

(defun %make-hash (numlist)
  (iter (with hash = (make-hash-table :test #'equal))
	(for n :in numlist)
	(setf (gethash n hash) 1)
	(finally (return hash))))

(defun target-zenken (meibo)
  (declare (type MeiboCSV meibo))
  (iter (with hash = (cl-store:restore *zenken-hash*))
	(for line :in (body-> meibo))
	(for num = (zero-padding (nth 14 line) 9))
	(optima:match (gethash num hash)
	  ((LIST* _ nil _)
	   (collect (make-instance 'MeiboLine :list line)
	     :into pot))
	  (_
	   (next-iteration)))
	(finally (return pot))))

(defun target (meibo numlist)
  (declare (type MeiboCSV meibo))
  (iter (with hash = (%make-hash numlist))
	(for line :in (body-> meibo))
	(print (zero-padding (nth 14 line) 9))))
