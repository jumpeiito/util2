(in-package :rcv)

(defparameter %file "y:/23吉田/文書受付簿/文書受付簿　2013年.xls")
(defparameter %vecfile #P"f:/util2/receive/receive.vec")

(defun contents (filename)
  (excel:with-excel (app :visible nil :quit t)
    (excel:with-excel-book (app bk filename :close t)
      (let* ((sh  (ole bk :worksheets :item "Sheet2"))
	     (urv (slot-value (ole sh :usedrange) 'value)))
	(iter (with prev)
	      (for line :in urv)
	      (unless (first-time-p)
		(optima:match line
		  ((list _ date sender cont _)
		   (if (or date sender cont)
		       (collect (vector (util::to-string (or date prev))
					sender cont) :into pot)
		       (leave pot))
		   (if date (setq prev date))))))))))

(defun output ()
  (with-open-file (op %vecfile
		      :direction :output
		      :if-exists :supersede
		      :external-format :utf-8)
    (write (contents %file) :stream op)))

(defun load-vec ()
  (with-open-file (i %vecfile :direction :input)
    (read i)))

(defun seek (&key (date nil) (keyword nil))
  (iter (for v :in (load-vec))
	(optima:match v
	  ((vector d s c)
	   (if (and (if date (date= (strdt date) (strdt d)) t)
	   	    (if keyword
	   		(or (ppcre:scan keyword s)
	   		    (ppcre:scan keyword c))
	   		t))
	       (collect v))))))
