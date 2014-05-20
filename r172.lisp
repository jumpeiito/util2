(in-package :R172c)

(defparameter 172file ksetting::*fkca172*)
(defparameter 167file ksetting::*fkac167*)

(defun make-local-hash (filename nth)
  (let ((hash (make-hash-table :test #'equal)))
    (csv-read-iter
     filename
     (lambda (line)
       (setf (gethash (nth nth line) hash) line)))
    hash))

(defun dock-hash ()
  (make-local-hash ksetting::*dock-output-file* 5))

(defun sc-hash ()
  (make-local-hash ksetting::*sc-output-file* 0))

(defun create-r167 (line)
  (make-r167 :jnum (nth 9 line)
	     :date (nth 10 line)
	     :hp   (nth 11 line)
	     :mlv  (nth 48 line)
	     :hlv  (nth 49 line)))

(defun 167file-read (func)
  (csv-read-iter
   167file
   (lambda (line)
     (optima:match line
       ((LIST _) nil)
       ((LIST* _ "FKAC167" _) nil)
       ((LIST* "保険者番号" _) nil)
       (_ (funcall func line))))
   :code (file-coding 167file)))

(defun 167file-hash ()
  (let ((hash (make-hash-table :test #'equal)))
    (167file-read
     (lambda (line)
       (let1 obj (create-r167 line)
	 (with-slots (jnum) obj
	   (setf (gethash jnum hash) obj)))))
    hash))

(defun data-iterate (func)
  (let ((zhash (cl-store:restore ksetting::*zenken-hash*))
	(167hash (167file-hash)))
    (csv-read-iter
     172file
     (lambda (line)
       (optima:match line
	 ((LIST _)	      :ignore)
	 ((LIST* _ "FKCA172" _) :ignore)
	 (_ (funcall func (create-172data line zhash 167hash)))))
     :code (file-coding 172file))))

(defun 172-hash ()
  (let ((hash (make-hash-table :test #'equal)))
    (data-iterate
     (lambda (d)
       (setf (gethash (172data-受診券整理番号 d) hash) d)))
    hash))

(defun 172-uploaded? (jnumber hash)
  (optima:match (gethash jnumber hash)
    ((172data 健診メッセージ 指導メッセージ)
     (if (and (util:string-null 健診メッセージ)
	      (util:string-null 指導メッセージ))
	 :uploaded
	 (format nil "~A~A" 健診メッセージ 指導メッセージ)))
    (nil :not-uploaded)))

(defun data-filter-map (func pred)
  (let ((zhash   (cl-store:restore ksetting::*zenken-hash*))
	(167hash (167file-hash)))
    (csv-read-filter-map
     172file
     ;; func部
     (lambda (line)
       (optima:match line
	 ((LIST _)	      :ignore)	; 末端行
	 ((LIST* _ "FKCA172" _) :ignore)	; 先頭行
	 (_ (funcall func (create-172data line zhash 167hash)))))
     pred :code :SJIS)))

(defun main-condition (172data)
  (if (typep 172data 'kensin::172data)
      (with-slots (資格フラグ 健診メッセージ 指導メッセージ) 172data
	(optima:match (list 資格フラグ 健診メッセージ 指導メッセージ)
	  ((LIST "0" "" "") t)
	  (_ nil)))
      nil))

(defun board-on-iterate (func)
  (data-iterate
   (lambda (r172)
     (if (main-condition r172)
	 (funcall func r172)))))

(defstruct counter shibu sc dock)

(defun create-counter ()
  (labels ((f () (make-array 3 :initial-element 0)))
    (let ((c (make-counter)))
      (with-slots (shibu sc dock) c
	(setf shibu (f)
	      sc    (f)
	      dock  (f))
	c))))

(defun sample ()
  (nth 100 (data-filter-map #'identity #'identity)))

(defmacro inner-calc-cond (&rest clause)
  `(cond
     ,@(mapcar (lambda (list)
		 (destructuring-bind (hash array r172 jnum) list
		   (if (eq hash t)
		       `(t
			 (aref-1+ ,array (if (h? ,r172) 0 1))
			 (aref-1+ ,array 2))
		       `((gethash ,jnum ,hash)
			 (aref-1+ ,array (if (h? ,r172) 0 1))
			 (aref-1+ ,array 2)))))
	       clause)))

(defun inner-calc (r172 counter dhash shash)
  (with-slots (shibu sc dock) counter
    (let ((jnum (172data-受診券整理番号 r172)))
      (inner-calc-cond
       (dhash dock r172 jnum)
       (shash sc r172 jnum)
       (t     shibu r172 jnum)))))

(defun calc ()
  (let ((c (create-counter))
	(dhash (r172c::dock-hash))
	(shash (r172c::sc-hash)))
    (board-on-iterate
     (lambda (r172)
       (inner-calc r172 c dhash shash)))
    c))

(defun mainhash ()
  (let ((hash (make-hash-table :test #'equal)))
    (data-iterate
     (lambda (l)
       (if (main-condition l)
	   (setf (gethash (172data-支部 l) hash)
		 (cons l (gethash (172data-支部 l) hash))))))
    hash))

(defun main-calc ()
  (let ((dhash (r172c::dock-hash))
	(shash (r172c::sc-hash)))
   (iter (for (shibu v) :in-hashtable (mainhash))
	 (collect (cons shibu (iter (with c = (create-counter))
				    (for r :in v)
				    (inner-calc r c dhash shash)
				    (finally (return c))))
	   :into pot)
	 (finally (return (alexandria:alist-hash-table pot :test #'equal))))))

(defmacro hash+ (key val hash)
  `(setf (gethash ,key ,hash)
	 (cons ,val (gethash ,key ,hash))))

(defun sex-year-count (list)
  (group-by-length
   list
   (lambda (d) (floor (/ (get-year d) 10)))))
(defun hash-map (func hash)
  (iter (for (k v) :in-hashtable hash)
	(setf (gethash k hash)
	      (funcall func v))
	(finally (return hash))))

(defun sex-year-classify-hash (list)
  (iter (with hash = (make-hash-table :test #'equal))
	(for d :in list)
	(hash+ :all d hash)
	(if (ppcre:scan "1$" (172data-受診券整理番号 d))
	    (hash+ :h d hash)
	    (hash+ :k d hash))
	(if (string= "1" (172data-性別 d))
	    (hash+ :m d hash)
	    (hash+ :f d hash))
	(finally (return (hash-map #'sex-year-count hash)))))

(defun make-sex-year-hash ()
  (alexandria:alist-hash-table
   (mapcar (lambda (l) (cons l (make-array 4 :initial-element 0)))
	   '(:all :h :k :m :f))
   :test #'equal))

(defun sex-year-classify-hash2 (list)
  (iter (with hash = (make-sex-year-hash))
	(for d :in list)
	))

(defun sex-year-classify (hash)
  (iter (for (k v) :in-hashtable hash)
	(setf (gethash k hash)
	      (sex-year-classify-hash v))
	(finally (return hash))))

(defpackage #:r172-uchiwake
  (:nicknames #:R172u)
  (:use :cl :util :kensin :iterate :excel :cl-win32ole)
 )

(in-package #:R172-UCHIWAKE)

(defparameter file "y:/68稲吉/理事会資料/0000.xlsx")

(defun shibulist ()
  (iter (for (k . v) :in kensin::short-shibu-alist)
	(if (string= k "85")
	    (next-iteration)
	    (collect k))))

(defun hk-multiple (list)
  (let ((l (group-by-length list #'zenken::zenken-本人／家族)))
    (flet ((f (key) (cdr (assoc key l :test #'equal))))
      (values (f "本人") (f "家族")))))

(defun main ()
  (with-excel (app :visible t :quit nil :debugger t)
    (with-excel-book (app book file :close nil :debugger t)
      (multiple-value-bind (20hash 40hash) (zenken::to-data-multiple)
	(let* ((sheet (ole book :Worksheets :Item 1))
	       (mainhash (r172c::main-calc)))
	  (iter (for shibu :in (shibulist))
		(for top :from 7 :by 3)
		(for second :from 8 :by 3)
		(for bottom :from 9 :by 3)
		(for counter = (gethash shibu mainhash))
		(excel::value! sheet (:d top)
			       (length (gethash shibu 20hash)))
		(multiple-value-bind (h k) (hk-multiple (gethash shibu 40hash))
		  (excel::value! sheet (:h top) h)
		  (excel::value! sheet (:h second) k)
		  (excel::value! sheet (:h bottom) (+ h k)))
		(with-slots (r172c::shibu r172c::sc r172c::dock) counter
		  (excel::value! sheet (:i top) (aref r172c::shibu 0))
		  (excel::value! sheet (:i second) (aref r172c::shibu 1))
		  (excel::value! sheet (:i bottom) (aref r172c::shibu 2))
		  (excel::value! sheet (:k top) (aref r172c::sc 0))
		  (excel::value! sheet (:k second) (aref r172c::sc 1))
		  (excel::value! sheet (:k bottom) (aref r172c::sc 2))
		  (excel::value! sheet (:m top) (aref r172c::dock 0))
		  (excel::value! sheet (:m second) (aref r172c::dock 1))
		  (excel::value! sheet (:m bottom) (aref r172c::dock 2)))))))))

