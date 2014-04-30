(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util))

(defpackage :rece
  (:nicknames :rece)
  (:use :cl :util :iterate :cl-win32ole :excel)
  (:export #:main)
  (:import-from #:optima #:match))

(in-package :rece)

(defclass CSV ()
  ((excel       :initarg :excel)
   (book	:initarg :book)
   (csvname     :initarg :csvname)
   (csvdata     :initarg :csvdata)
   (title	:initarg :title)
   (xlsname	:initarg :xlsname)
   (sheet	:initarg :sheet :reader sheet)
   (lastrow	:initarg :lr)
   (lastcol	:initarg :lc)
   (columns     :initarg :columns)
   (width	:initarg :width)
   (condition   :initarg :condition)
   (remove      :initarg :remove)
   (outdate     :initarg :outdate)
   (formula	:initarg :formula
		:initform '(:K :M))))

(defclass CSV-type1 (CSV)
  ((columns	:initarg :columns
		:initform '(1 2 3 4 5 7 8 9 10 20))
   (width	:initarg :width
		:initform
		#(5 12 6.5 10 6.5 15 13 30 9 9 17
		  10 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5))
   (align       :initarg :align
		:initform
		#(nil :center :center :center nil nil :center
		  nil nil nil :center :center :center :center
		  :center :center :center :center
		  :center :center :center :center))
   (insert	:initarg :insert
		:initform '("J:K"))
   (numberformat :initarg :numberformat
		 :initform '("I:J"))
   (birthday    :initarg :birthday
		:initform :L)
   (mergecell   :initarg :mergecell
		:initform "M1:V1")
   (header      :initarg :header
		:initform '((:J . "請求点数")
			    (:K . "包番号")
			    (:M . "新包番号")))))

(defclass CSV-type2 (CSV)
  ((columns	:initarg :columns
		:initform '(1 2 3 4 5 7 8 9 10 20))
   (width	:initarg :width
		:initform
		#(5 12 6.5 10 6.5 15 13 30 9 9 17
		  10 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5))
   (align       :initarg :align
		:initform
		#(nil :center :center :center nil nil :center
		  nil nil nil :center :center :center :center
		  :center :center :center :center
		  :center :center :center :center))
   (insert	:initarg :insert
		:initform '("J:K"))
   (numberformat :initarg :numberformat
		 :initform '("I:J"))
   (birthday    :initarg :birthday
		:initform :L)
   (mergecell   :initarg :mergecell
		:initform "M1:V1")
   (header      :initarg :header
		:initform '((:I . "請求点数")
			    (:J . "決定点数")
			    (:K . "包番号")
			    (:M . "新包番号")))))

(defun rece-date (list)
  (aif (strdt (last1 list))
       (format nil "~A ~A ~A"
	       (date-year it)
	       (date-month it)
	       (date-day it))
       (last1 list)))

(defun csvdata (csvfile)
  (iter (for line :in-csv csvfile :code :UTF-8)
	(match line
	  ((list* a b c d e _ f g h i j)
	   (collect (list a b c d e f g h i
			  (rece-date j)))))))

(defun compress (number-list)
  (labels ((in (subl start plus r)
	     (if (null subl)
		 (aif (reverse (cons (list start plus) r))
		      (mapcar 
		       (lambda (l)
			 (optima:match l
			   ((LIST start end)
			    (if (eq end 1)
				(format nil "~A:~:*~A" start)
				(format nil "~A:~A" start (1- (+ start end)))))))
		       it))
		 (if (eq (car subl) (+ start plus))
		     (in (cdr subl) start (1+ plus) r)
		     (in (cdr subl) (car subl) 1 (cons (list start plus) r))))))
    (if number-list
	(in number-list (car number-list) 0 nil)
	nil)))

(defun remove-lines (csvdata)
  (iter (for line :in csvdata)
  	(for row :upfrom 2)
	(match line
	  ((or (LIST* _ _ "原審" _)
	       (LIST* _ _ "再審査返戻" _)
	       (LIST* _ _ "差戻" _))
	   (collect row)))))

(defgeneric month-gap (date1 date2))
(defmethod  month-gap ((d1 DT:DATE-TIME) (d2 DT:DATE-TIME))
  (let1 gap (mapcar #'-
		    (list (dt:year-of d1) (dt:month-of d1))
		    (list (dt:year-of d2) (dt:month-of d2)))
    (optima:match gap
      ((LIST year month)
       (+ (* 12 year) month)))))
(defmethod  month-gap ((d1 LOCAL-TIME:TIMESTAMP) (d2 LOCAL-TIME:TIMESTAMP))
  (let1 gap (mapcar #'-
		    (list (date-year d1) (date-month d1))
		    (list (date-year d2) (date-month d2)))
    (optima:match gap
      ((LIST year month)
       (+ (* 12 year) month)))))
(defmethod  month-gap ((d1 STRING) (d2 STRING))
  (month-gap (strdt d1) (strdt d2)))
(defmethod  month-gap ((d1 STRING) (d2 LOCAL-TIME:TIMESTAMP))
  (month-gap (strdt d1) d2))

(defun out-date-p (string)
  (> (month-gap (local-time:today) (strdt string)) 4))

;;; レセ取消月より4ヶ月前に受診するものが大半。それ以外のものに色をつける。
(defun out-date (csvdata)
  (iter (for line :in csvdata)
	(for row :upfrom 2)
	(optima:match line
	  ((LIST* _ _ _ _ _ _ _ month _)
	   (if (out-date-p month)
	       (collect row))))))

(defun ColumnDelete (csv)
  (with-slots (columns sheet) csv
    (iter (for i :from 19 :downto 1)
	  (unless (member i columns :test #'equal)
	    (ole sheet
		 :range (format nil "~A:~:*~A" (excel::number-to-col i))
		 :delete)))))

(defun ColumnInsert (csv)
  (with-slots (insert sheet) csv
    (iter (for range :in insert)
    	  (ole sheet :range range :Insert))))

(defun SetWidth (csv)
  (with-slots (width sheet) csv
    (excel::set-colwidth sheet width)))

(defun BirthdayRepair.calc (dt)
  (format nil "~A ~2,'0d ~2,'0d"
	  (dt:year-of dt)
	  (dt:month-of dt)
	  (dt:day-of dt)))

(defun BirthdayRepair (csv)
  (with-slots (birthday sheet) csv
    (let1 lr (lastrow sheet)
      (let1 value (value sheet (birthday 2) (birthday lr))
	(excel::value! sheet
		       (birthday 2) (birthday lr)
		       (mapcar (compose #'list #'BirthdayRepair.calc)
			       (mapcar #'car value)))))))

(defun SetAlign (csv)
  (with-slots (align sheet lastrow) csv
    (iter (for a :in-vector align)
	  (for i :upfrom 1)
	  (when a
	    (let1 col (excel::number-to-col i)
	      (set-alignment sheet (col 2) (col lastrow)
			     :horizontalAlignment excel::xlcenter))))))

(defun SetNumberFormat (csv)
  (with-slots (numberformat sheet) csv
    (iter (for range :in numberformat)
    	  (set-number-format-local (ole sheet :range range)
				   "#,##"))))

(defun SetFont (csv)
  (with-slots (sheet lastrow) csv
    (set-fontname sheet (:a 1) (:v lastrow)
		  "ＭＳ Ｐ明朝")
    (set-fontname sheet (:a 1) (:v lastrow)
		  "Times New Roman")))

(defun SetBorder (csv)
  (with-slots (sheet lastrow) csv
    (border sheet (:a 1) (:v lastrow))))

(defun MergeCells (csv)
  (with-slots (sheet mergecell) csv
    (setf (slot-value (ole sheet :range mergecell)
		      :MergeCells)
	  t)))

(defun TitleRepair (csv)
  (with-slots (sheet header) csv
    (iter (for (col . title) :in header)
	  (excel::value! sheet (col 1) title))
    (set-alignment sheet (:a 1) (:v 1)
		   :horizontalAlignment excel::xlcenter)))

;;; (compress '(1 2 3 4 6 8 9))
;;; -> '((1 2 3 4) 6 (8 9))
(defun MainOperate (csv)
  (with-slots (sheet lastrow remove outdate) csv
    (if outdate
	(iter (for range :in outdate)
	      (setf (slot-value (ole sheet :range range :Interior)
				:ColorIndex)
		    excel::xlgray25)))
    (iter (for range :in (reverse remove))
    	  (ole sheet :range range :delete))
    (setf (slot-value (ole sheet :range (format nil "~A:~A" 2 (lastrow sheet)))
		      :RowHeight)
	  20)))

(defun PutFormula (csv)
  (with-slots (sheet lastrow formula) csv
    (optima:match formula
      ((LIST old new)
       (iter (for row :from 2 :to lastrow)
	     (setf (slot-value (range sheet (new row)) :value)
		   (format nil "=LEFT(~A~A,1)" old row)))))))

(defun SetPage (csv)
  (with-slots (sheet lastrow) csv
    (excel::PageSetup
     sheet
     :PrintArea		(format nil "A1:V~A" (lastrow sheet))
     :Orientation	excel::xlLandscape
     :PaperSize		excel::xlB4
     :PrintTitleRows	"$1:$1"
     :Zoom		nil
     :FitToPagesTall	nil
     :FitToPagesWide	1)))

(defun putnumber-data (lastrow)
  (mapcar
   (lambda (_)
     (declare (ignorable _))
     (let1 today (dt:today)
       (explode (format nil "~2,'0d~2,'0d90"
			(mod (dt:year-of today) 100) (dt:month-of today)))))
   (iota :from 2 :to lastrow)))

(defun PutNumber (csv)
  (with-slots (sheet) csv
    (let1 lr (lastrow sheet)
      (let1 val (value sheet (:d 2) (:d lr))
	(excel::value! sheet (:d 2) (:d lr)
		       (mapcar (compose #'list #'to-hankaku)
			       (mapcar #'car val))))
      (decide-range-value sheet
			  (putnumber-data lr)
			  :start-row 2
			  :start-column 14))))

(defmethod initialize-instance :after ((c CSV) &rest args)
  (declare (ignorable args))
  (with-slots (csvname csvdata xlsname book sheet lastrow lastcol title remove outdate) c
    (let1 init (csv-read-to-list csvname)
      (setq sheet	(ole book :worksheets :item 1)
	    xlsname	(make-pathname :defaults csvname
				       :type "xls")
	    csvdata	(cdr init)
	    title	(car init)
	    lastrow	(1+ (length csvdata))
	    remove      (compress (remove-lines csvdata))
	    outdate     (compress (out-date csvdata)))
      (ColumnDelete    c)
      (ColumnInsert    c)
      (SetWidth        c)
      (SetAlign        c)
      (BirthdayRepair  c)
      (SetNumberFormat c)
      (SetBorder       c)
      (MergeCells      c)
      (TitleRepair     c)
      (MainOperate     c)
      (PutFormula      c)
      (SetPage	       c)
      (PutNumber       c)
      (SetFont         c))))

(defun main (filename)
  (with-excel (app :visible t :quit nil :debugger t)
    (with-excel-book (app book filename :close nil :debugger t)
      (let1 obj (make-instance (rxmatch-case (namestring filename)
					     ("(.+再審.+)" (p) 'CSV-type1)
					     ("(.+調剤.+)" (p) 'CSV-type2))
			       :excel   app
			       :book    book
			       :csvname filename)
	obj))))

(in-package :cl-user)
