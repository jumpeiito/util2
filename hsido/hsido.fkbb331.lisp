(in-package #:hfk)

(defparameter dir #P"d:/特定健診システム/特定健診利用券情報/")
(defparameter file (merge-pathnames dir "FKBB331.CSV"))

(defun to-hash (list)
  (alexandria:alist-hash-table
   (mapcar
    (lambda (line) (cons (nth 9 line) line))
    list)
   :test #'equal))

(defun info ()
  (iter (with header = nil)
	(for line :in (csv-read-to-list file :code :SJIS))
	(optima:match line
	  ((LIST* _ "FKBB331" _) (setf header line))
	  ((LIST _) (next-iteration))
	  ((TYPE LIST) (collect line :into pot)))
	(finally (return (values header (to-hash pot))))))

(defun make-body (jnumber-list hash)
  (iter (for jnum :in jnumber-list)
	(aif (gethash jnum hash)
	     (collect it)
	     (progn
	       (warn "This jnumber(~A) may be inappropriate." jnum)
	       (next-iteration)))))

(defun make-filename ()
  (merge-pathnames dir
		   (format nil "00263129_FKBB331_~A_001.csv" (util::today-8))))

(defun make-csv (jnumber-list)
  (multiple-value-bind (header hash) (info)
    (let ((body (make-body jnumber-list hash)))
      (call-with-output-file2 (make-filename)
	(lambda (op)
	  (format op "~{~A~^,~}~%" header)
	  (format op "~{~{~A~^,~}~%~}" body)
	  (format op "~A" (length body)))
	:code :SJIS))))
