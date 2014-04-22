(defpackage #:jnu
  (:use #:cl #:util #:kensin #:iterate #:cl-win32ole #:excel))
(in-package #:jnu)

;; (defparameter f "Y:/23吉田/支部健康診断/2013.xls")

;; (defparameter l '("13.4" "13.5" "13.6" "13.7" "13.8" "13.9" "13.10" "13.11" "13.12"))

;; (defun x ()
;;   (with-excel (app :visible t :quit nil)
;;     (with-excel-book (app bk f :close nil)
;;       (iter (for sh :in l)
;; 	    (let* ((sheet (ole bk :Worksheets :Item sh))
;; 		   (lr (lastrow sheet :y 6)))
;; 	      (appending
;; 	       (mapcar (lambda (line)
;; 			 (append line (list sh)))
;; 		       (value sheet (:a 6) (:d lr)))))))))

;; (defun y ()
;;   (call-with-input-file2 "hashi.vec" #'read))

;; (defparameter hash
;;   (alexandria:alist-hash-table
;;    `(("表具" . "西七条診療所")
;;      ("綴喜八幡支部" . "城南診療所")
;;      ("洛南支部" . "城南診療所")
;;      ("相楽支部" . "城南診療所")
;;      ("奥丹後支部" . "たんご協立診療所")
;;      ("宮津支部" . "城南診療所")
;;      ("舞鶴支部" . "まいづる協立診療所")
;;      ("福知山支部" . "城南診療所")
;;      ("綾部支部" . "京都協立病院")
;;      ("船井支部" . "城南診療所")
;;      ("亀岡支部" . "城南診療所")
;;      ("宇治支部" . "工場保健会")
;;      ("乙訓支部" . "")
;;      ("醍醐支部" . "城南診療所")
;;      ("伏見支部" . "城南診療所")
;;      ("西京支部" . "城南診療所")
;;      ("右京支部" . "太子道診療所")
;;      ("山科支部" . "大宅診療所")
;;      ("東山支部" . "東山診療所")
;;      ("左京支部" . "")
;;      ("南支部" . "吉祥院病院")
;;      ("下京支部" . "西七条診療所")
;;      ("中京支部" . "太子道診療所")
;;      ("上京支部" . "上京診療所")
;;      ("北支部" . "上京診療所"))
;;    :test #'equal))

;; (defun z ()
;;   (iter (for line :in (y))
;; 	(optima:match line
;; 	  ((LIST shibu h k total mon)
;; 	   (aif (gethash shibu hash)
;; 		(collect (list shibu "" "-" "" (or h "") (or k "") "" total it mon))
;; 		(if (ppcre:scan "電気工事組合.*[(（](.+)[）)]" shibu)
;; 		    (ppcre:register-groups-bind (hosp) ("電気工事組合.*[(（](.+)[）)]" shibu)
;; 		      (collect (list "電工" "" "-" "" (or h "") (or k "") "" total hosp mon)))
;; 		    (collect (list shibu "" "-" "" (or h "") (or k "") "" total "" mon))))))))

;; (defun z2 ()
;;   (with-excel (app :visible t :quit nil)
;;     (with-excel-book (app bk f :close nil)
;;       (let ((sh (ole bk :Worksheets :add)))
;; 	(decide-range-value sh (z) :start-row 2)))))

(defun d ()
  (with-excel (app :visible t :quit nil)
    (with-excel-book (app book "g:/00263129-201310-25.xls" :close nil)
      (let* ((sh  (ole book :WorkSheets :Item 1))
	     (lr  (lastrow sh))
	     (val (value sh (:a 2) (:f lr))))
	;; (call-with-output-file2 "hashi.vec"
	;;   (lambda (op) (write val :stream op)))
	(iter (for row :from 2 :to lr)
	      (for id = (value sh (:f row)))
	      (optima:match (gethash id zh)
		((type NULL)
		 ;; (collect line)
		 (set-colorindex sh (:a row) (:n row) :interior excel::xlyellow))
		((LIST nil nil)
		 (next-iteration))
		(_
		 ;; (collect line)
		 (set-colorindex sh (:f row) (:n row) :interior excel::xlyellow))))))))

(defun dr ()
  (call-with-input-file2 "hashi.vec" #'read))

;; (defparameter zh (zenken::id-hash2 "f:/20130628/特定健診全件データ.csv" 0))

(defun dq ()
  (iter (for line :in (dr))
	(for id = (sixth line))
	(optima:match (gethash id zh)
	  ((type NULL)
	   (collect line))
	  ((LIST nil nil)
	   (next-iteration))
	  (_
	   (collect line)))))
(defvar file "f:/util2/20131216 乙訓除外.csv")
(defvar file2 #P"f:/util2/国保伊東20131217 乙訓支部受診券発送者.CSV")

(defun hash ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in (cdr (csv-read-to-list file :code :SJIS)))
	(setf (gethash (car line) hash)
	      1)
	(finally (return hash))))

(defun file2 ()
  (iter (with hash = (hash))
	(for line :in (csv-read-to-list file2 :code :SJIS))
	(if (gethash (car line) hash)
	    (cl-match:match line
	      ((LIST* jnum name _ _ _ _ k b _ ad pos _ _ _ num _)
	       ;; (format t "~{~A~^,~}~%"
	       ;; 	       (list jnum name k b ad pos num))
	       (collect (list jnum name k b ad pos num)))))))

(defun expose ()
  (iter (for line :in (sort (file2) (lambda (x y) (< (read-from-string (last1 x))
						     (read-from-string (last1 y))))))
	(format t "~{~A~^,~}~%" line)))
