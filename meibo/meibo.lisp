(defpackage #:meibo
  (:nicknames :mb)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel))

(in-package :mb)

(defvar *year*		ksetting::*year*)
(defvar *nendo-end*	(nendo-end *year*))
(defvar *file*		ksetting::*zenken-file*)
(defvar *zenken-csv*	#P"f:/zenken.csv")
(defvar *zenken-hash*	#P"f:/zenken.hash")

(defun %to-zenken ()
  (iter (with bhash = (kensin:bunkai-hash))
	(for line :in-csv *file* :code :SJIS)
	(for z = (zenken::create-zenken line bhash))
	(if (and (not (first-time-p))
		 (zenken::target? z ))
	    (collect z))))

(defun make-zenken-store ()
  (cl-store:store (%to-zenken) *zenken-csv*))

(defun zenken-store ()
  (unless (cl-fad:file-exists-p *zenken-csv*)
    (make-zenken-store))
  (cl-store:restore *zenken-csv*))

;; f:/util2/meibo/特定健診用氏名一覧56綾_20131127.csv
(defun number-for-sort (line)
  (optima:match line
    ((LIST* jnum _ _ _ _ _ _ _ _ _ _ _ telnum _)
     ;; (format nil "~A~A" telnum jnum)
     telnum)))

(defun kill-blank (string)
  (ppcre:regex-replace-all "[　 ]{2,}" string ""))

(defun kill-blank-literally (string)
  (ppcre:regex-replace-all "[　 ]+" string ""))

(defun line-for-print (line bk)
  (optima:match line
    ((LIST* _ name furi gender birthday year k b hk ad pos tel _)
     (format nil "~{~A~^,~}"
	     (list k b bk name
		   (kill-blank furi)
		   hk gender birthday year ;; pos
		   (kill-blank-literally ad)
		   tel)))))

(defun yet? (zenken)
  (with-slots (zenken::受診日 zenken::発行日) zenken
    (and (not zenken::受診日) (not zenken::発行日))))

(defun %csv-sort (list)
  (sort2 list string< number-for-sort))

(defun on-csv? (zenken-obj)
  (and zenken-obj (zenken::target? zenken-obj) (yet? zenken-obj)))

(defun %csv (filename)
  (let (title)
    (iter (with zhash = (cl-store:restore *zenken-hash*))
	  (for line :in-csv filename :code :SJIS)
	  (if (first-time-p)
	      (progn
		(setq title line)
		(next-iteration)))
	  (optima:match line
	    ((LIST* jnum _)
	     (for obj = (gethash jnum zhash))
	     (if (on-csv? obj)
		 (collect (cons (zenken::zenken-分会名 obj) line)
		   :into pot))))
	  (finally (return (append (list (line-for-print title "分会名"))
				   (mapcar (lambda (c)
					     (line-for-print (cdr c) (car c)))
					   (%csv-sort pot))))))))

;; 特定健診用氏名一覧16東_20131127.csv
(defun filename-parse (filename)
  (let ((f (pathname-name
	    (make-pathname :defaults filename))))
    (cl-ppcre:register-groups-bind
    	(code shibu)
    	("特定健診用氏名一覧(\\d{2})(.)_\\d{8}.csv" f)
      (values (read-from-string code)
    	      shibu))))

(defun %csv-newname (filename)
  (multiple-value-bind (code shibu)
      (filename-parse filename)
    (make-pathname :defaults filename
		   :name (format nil "電話名簿~A~A" code shibu))))

(defun %csv-output (filename)
  (call-with-output-file2 (%csv-newname filename)
    (lambda (op)
      (iter (for line :in (%csv filename))
	    (write-line line op)))
    :code :SJIS))

;; #(10 6 9.5 12.5 12.5 12.5 5 5 11 6 12)

(defparameter xls-width #(10 6 9.5 12.5 12.5 5 5 11 6 32 13))

(defun %xls-width (sh)
  (excel::set-colwidth sh xls-width))

(defun %xls-align (sh)
  (loop
     :for row
     :in '(:a :b :c :f :g :i)
     :do  (setf (slot-value (ole sh :range (format nil "~A:~:*~A" row))
			    :horizontalAlignment)
		excel::xlcenter)))

(defun row-topline-erase (sheet row)
  (setf (slot-value (ole sheet :range (format nil "A~A:K~:*~A" row) :borders 8)
			    :linestyle)
		nil))

;; (iter (generating line :in x)
;;       (if (equal (last1 line) (last1 (next line)))
;; 	  (print line)))

(defun topline-erase (sh uv)
  (iter (generating line :in uv)
	(for row :upfrom 1)
	(if (equal (last1 line) (last1 (next line)))
	    (row-topline-erase sh row))))

(defun %xls (filename)
  (%csv-output filename)
  (let ((newname (namestring (%csv-newname filename))))
    (with-excel (app :visible t :quit nil)
      (with-excel-book (app book newname :close nil)
	(let* ((sh (ole book :Worksheets :item 1))
	       (lr (lastrow sh))
	       (uv (value sh (:a 1) (:k lr))))
	  (%xls-width sh)
	  (%xls-align sh)
	  (borders sh :a 1 :k lr)
	  (topline-erase sh uv)
	  (set-alignment sh (:a 1) (:k 1)))))))
