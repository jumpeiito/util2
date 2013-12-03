(in-package :kensin)

;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   (require :util)
;;   (use-package :util)
;;   (use-package :iterate))

(defvar 167file #P"F:/FKAC167.csv")
(defvar 167file-2011 #P"F:/FKAC167_2011.csv")

(defstruct r167
  記号 番号 生年月日 性別 個人番号 整理番号 実施年月日 健診機関コード
  BMI メタボレベル 保健指導レベル 健診機関 年齢 支部 実施年 実施月)

(defmacro r167-setq (&rest param)
  `(setq ,@(mapcar-with-index
	    (lambda (c l) (if (eq (mod c 2) 1)
			      (if (listp l)
				  `(funcall ,(second l) (nth ,(first l) line))
				  `(nth ,l line))
			      l))
	    param)))

(defun r167-date (datestring)
  (normal-date->string (strdt datestring)))

(defun r167-gender (flag)
  (string-case flag
    ("1" "男")
    ("2" "女")
    (t "")))

(defun r167-lv (flag)
  (string-case flag
    ("1" (list 1 "積極的支援"))
    ("2" (list 2 "動機付支援"))
    ("3" (list 3 "情報提供"))
    ("4" (list 4 "なし"))
    (t (list 0 ""))))

(defun r167-metabo-lv (flag)
  (string-case flag
    ("1" (list 1 "基準該当"))
    ("2" (list 2 "予備群該当"))
    ("3" (list 3 "情報提供"))
    ("4" (list 4 "なし"))
    (t (list 0 ""))))

(defmacro with-r167 (obj &rest body)
  `(with-slots (記号 番号 生年月日 性別 個人番号 整理番号 実施年月日 健診機関コード
		      BMI メタボレベル 保健指導レベル 健診機関 年齢 支部 実施年 実施月) ,obj
     ,@body))

(defun create-r167 (line)
  (let1 obj (make-r167)
    (with-r167 obj
      (r167-setq 記号		2
		 番号		3
		 生年月日	(5  #'r167-date)
		 性別		(6  #'r167-gender)
		 個人番号	7
		 整理番号	9
		 実施年月日	(10 #'r167-date)
		 健診機関コード	11
		 BMI		14
		 メタボレベル	(48 #'r167-metabo-lv)
		 保健指導レベル	(49 #'r167-lv)
		 健診機関	(11 #'code->hospital)
		 年齢		(5 (lambda (birth) (how-old birth (nendo-end 2012)))))
      (setq 支部	(let1 s (subseq 整理番号 3 5)
			  (if (equal "85" s)
			      (if (equal "８５" (subseq 記号 4 6))
				  (to-hankaku (subseq 番号 0 2))
				  (to-hankaku (subseq 記号 4 6))) s))
	    実施年	(date-year  (string->date 実施年月日))
	    実施月	(date-month (string->date 実施年月日)))
      obj)))

(defun 167init (file)
  (declare (optimize speed)
	   (type simple-string file))
  (iter (for line :in-csv file :code :SJIS)
	(for row :upfrom 0)
	(declare (type fixnum row))
	(if (< row 2)
	    (next-iteration)
	    (collect (create-r167 line)))))

;; hash
(defun r167-hash (file)
  (declare (optimize speed))
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in-csv file :code :SJIS)
	(for row :upfrom 0)
	(declare (type fixnum row))
	(optima:match line
	  ((LIST* _ "FKAC167" _)  (next-iteration))
	  ((LIST* "保険者番号" _) (next-iteration))
	  ((LIST _)               (next-iteration))
	  ((LIST* "00263129" _)
	   (let ((obj (create-r167 line)))
	     (setf (gethash (r167-整理番号 obj) hash)
		   obj))))
	(finally (return hash))))


;; totalizer
(defun 167-shibu-class (file)
  (iter (for l :in (167init file))
	(phash l
	       :condition t
	       :key #'r167-支部)))

(defun month-classify (list)
  (iter (for r167 :in list)
	(phash r167
	       :condition t
	       :key (lambda (r167) (list (r167-実施年 r167) (r167-実施月 r167))))))

(defun 167-month-class (file)
  (iter (for (k v) :in-hashtable (167-shibu-class file))
	(collect (list k (month-classify v)))))

(defun 167-year-list ()
  (let1 dy (read-from-string ks::year)
    (append (iter (for month :from 4 :to 12) (collect (list dy month)))
	    (iter (for month :from 1 :to 3) (collect (list (1+ dy) month))))))

(defun 167-total (file)
  (iter (with yl = (167-year-list))
	(for (shibu hash) :in (sort2 (167-month-class file) string< car))
	(for total = (mapcar (lambda (y) (length (gethash y hash nil))) yl))
	;; (print (list shibu total (apply #'+ total)))
	(collect (list shibu total (apply #'+ total)))))

(in-package :cl-user)
