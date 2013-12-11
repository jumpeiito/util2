(defpackage #:meibo
  (:nicknames :mb)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel))

(in-package :mb)

(defvar *year*		ksetting::*year*)
(defvar *nendo-end*	(nendo-end *year*))
(defvar *file*		ksetting::*zenken-file*)
(defvar *zenken-csv*	#P"f:/zenken.csv")
(defvar *zenken-hash*	#P"f:/zenken.hash")
(defvar *dock*		#P"f:/util2/kserv/.dock")
(defvar *sc*		#P"f:/util2/kserv/.setcontract")

;; (A) 全件データCSVで把握可能なもの
;;    1. 特定健診用氏名一覧CSVから読み込み
;;    2. 全件データCSVで受診日が入っているものを除外
;;    3. ソート
;;       基本的に記号番号で並べかえるが、電話番号が一緒のものも同時に表示できるようにする。
;;       したがって、記号番号順に並んでいないものが一部存在する。
;; (B) 全件データCSVで把握不可能なもの
;;    1. 特定健診用氏名一覧CSVから読み込み
;;    2. 支部名簿などから、受診者の受診券整理番号を抽出
;;    3. ドック・集合契約名簿から、該当支部分の受診券整理番号を抽出
;;    4. 2.3.の受診券整理番号の分を除外
;;    5. ソート
;;       基準は上記といっしょ。

;; 電話番号が空欄のときに出力するかどうか
(defvar output-if-telnum-is-blanked nil)

(defstruct SHIBU pathname code name csv title clojure)

(defstruct (LINE
	     (:constructor line (jnum name furigana gender birthday year
				      k b hk ad postal telnum knumber ig1 id ig2)))
  jnum name furigana gender birthday year k b hk ad
  postal telnum knumber ig1 id ig2 zenken hnum)


(defun kill-blank-literally (string)
  (ppcre:regex-replace-all "[　 ]+" string ""))

(defun make-line (list zhash)
  (let1 obj (apply #'line list)
    (with-slots (ad jnum zenken hnum) obj
      (setq ad		(kill-blank-literally ad)
	    zenken	(gethash jnum zhash)
	    hnum	(read-from-string
			 (zenken::zenken-保険証番号 zenken))))
    obj))

(defun line-bunkai (line)
  (or (zenken::zenken-分会名 (line-zenken line))
      ""))

(defun line-bunkai-code (line)
  (zenken::zenken-分会 (line-zenken line)))

(defun filename-parse (filename)
  (let ((f (pathname-name
	    (make-pathname :defaults filename))))
    (cl-ppcre:register-groups-bind
    	(code shibu)
    	("特定健診用氏名一覧(\\d{2})(.)_\\d{8}" f)
      (values (read-from-string code)
    	      (kensin::long-shibu code)))))

(defun clojure-sort-key (line)
  (with-slots (zenken) line
    (with-slots (zenken::支部
		 zenken::分会
		 zenken::班
		 zenken::保険証番号
		 zenken::行) zenken
      (format nil "~A~A~A~A~A"
	      zenken::支部 zenken::分会 zenken::班 zenken::保険証番号 zenken::行))))

(defun list-output-sort-key (list)
  (optima:match list
    ((LIST* _ head _)
     (clojure-sort-key head))))

(defun make-shibu-reader-list (hash)
  (sort (alexandria:hash-table-alist hash)
	(lambda (x y)
	  (string< (list-output-sort-key x)
		   (list-output-sort-key y)))))

(def-clojure shibu-reader ()
  ((hash	(make-hash-table :test #'equal)))
  (:add		(line)
		(let1 keyword (line-telnum line)
		  (setf (gethash keyword hash)
			(aif (gethash keyword hash)
			     (sort (copy-list (cons line it))
				   (lambda (x y)
				     (string< (clojure-sort-key x)
					      (clojure-sort-key y))))
			     (list line)))))
  (:list	()
		(iter (for (telnum . data) :in (make-shibu-reader-list hash))
		      (if (if output-if-telnum-is-blanked
			      t
			      (ppcre:scan "^[0-9-]+$" telnum))
			  (appending data)))))

(defun target? (obj)
  (and obj (zenken::target? obj)
       (not (zenken::zenken-受診日 obj))
       (not (zenken::zenken-発行日 obj))))

(defun make-furigana-hash (list)
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in list)
	(optima:match line
	  ((LIST* _ _ furi _ _ _ k b hk _)
	   (if (equal hk "本人")
	       (setf (gethash (format nil "~A~A" k b) hash)
		     (car (ppcre:split " " furi))))))
	(finally (return hash))))

(defun furigana (line hash)
  (optima:match line
    ((LIST* _ _ furi _ _ _ k b _)
     (if (ppcre:scan "^ +$" furi)
  	 (setf (nth 2 line)
  	       (gethash (format nil "~A~A" k b) hash)))
     line)))

(defun exception (filename)
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in-file filename :using #'read-line)
	(setf (gethash (ppcre:regex-replace-all "" line "") hash)
	      1)
	(finally (return hash))))

(defun make-hash (file fn)
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in-csv file :code :UTF-8)
	(setf (gethash (funcall fn line) hash) 1)
	(finally (return hash))))

(defun dock ()
  (make-hash ksetting::*dock-output-file* #'sixth))

(defun sc ()
  (make-hash ksetting::*sc-output-file* #'first))

(defun %toCSV (list &optional exception)
  (iter (with hash = (cl-store:restore #P"f:/zenken.hash"))
	(with dock = (dock))
	(with sc   = (sc))
	(with furihash = (make-furigana-hash list))
	(for line :in list)
	(for jnum  = (car line))
	(for obj   = (gethash jnum hash))
	(if (and (target? obj)
		 (if exception
		     (not (or (gethash jnum exception)
			      (gethash jnum dock)
			      (gethash jnum sc)))
		     t))
	    (collect (make-line (furigana line furihash)
				hash)))))

(defun create-shibu-function (csv)
  (let1 f (shibu-reader)
    (dolist (line csv) (funcall f :add line))
    f))

(defun create-shibu (pathname &optional exception)
  (let1 obj (make-shibu :pathname pathname)
    (let1 init (csv-read-to-list pathname :code :SJIS)
      (multiple-value-bind (vcode vname)
	  (filename-parse pathname)
	(with-slots (pathname code name csv title clojure) obj
	  (setq code	vcode
		name	vname
		csv	(%toCSV (cdr init) exception)
		title	(car init)
		clojure	(create-shibu-function csv)))))
    obj))

(defgeneric expose (s))

(defmethod expose ((line LINE))
  (with-slots (name furigana gender birthday year k b hk ad telnum) line
    (list (line-bunkai line)
	  k b name furigana gender hk birthday year ad telnum)))

(defmethod expose ((shibu SHIBU))
  (with-slots (clojure) shibu
    (iter (for line :in (funcall clojure :list))
	  (collect (expose line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter width #(9 10 6 15 13 5 5 11 5.5 30 13 8 25))

(defun %PutData (sheet thread)
  (decide-range-value
   sheet `(("分会" "記号" "番号" "氏名" "フリガナ" "性別" "区分"
		   "生年月日" "年齢" "住所" "電話" "対応コード" "備考(要望・不満など具体的に)")))
  (decide-range-value
   sheet (sb-thread:join-thread thread)
   :start-row 2))

(defun row-for-erase (thread)
  (iter (generating line :in (sb-thread:join-thread thread))
	(for row :upfrom 2)
	(if (equal (nth 10 line) (nth 10 (next line)))
	    (collect row))))

(defun %row-for-merge (thread)
  (iter (with ary = (make-array '(5000) :initial-element 0))
	(for row :in (util::compact-str (row-for-erase thread)))
	(optima:match row
	  ((type ATOM)
	   (aref-1+ ary row))
	  ((LIST start end)
	   (iter (for i :from (1+ start) :to end)
		 (aref-1+ ary i))))
	(finally (return ary))))

(defun on-merge? (num ary)
  (eq 1 (svref ary num)))

(defun seek-not-on-merge (num ary)
  (iter (for i :downfrom num)
	(cond
	  ((eq i 0)
	   (leave nil))
	  ((eq 0 (svref ary i))
	   (leave i))
	  (t
	   (next-iteration)))))

(defun pagebreak (thread)
  (let* ((data   (sb-thread:join-thread thread))
	 (array  (%row-for-merge thread))
	 (length (length data)))
    (labels ((inner (counter r)
	       (if (> counter length)
		   (reverse r)
		   (let ((not-on-merge (seek-not-on-merge (+ 49 counter) array)))
		     (inner not-on-merge (cons not-on-merge r))))))
      (inner 2 nil))))

(defun row-for-merge (thread)
  (iter (for row :in (util::compact-str (row-for-erase thread)))
	(appending
	 (optima:match row
	   ((type ATOM)
	    (list (format nil "K~A:K~A" (1- row) row)
		  (format nil "L~A:L~A" (1- row) row)
		  (format nil "M~A:M~A" (1- row) row)))
	   ((LIST start end)
	    (list (format nil "K~A:K~A" (1- start) end)
		  (format nil "L~A:L~A" (1- start) end)
		  (format nil "M~A:M~A" (1- start) end)))))))

(defun line-erase (sheet thread)
  (iter (for row :in (row-for-erase thread))
	(setf (slot-value (ole sheet :range (format nil "A~A:K~:*~A" row) :borders 8)
			  :LineStyle)
	      nil)))

(defun line-merge (sheet thread)
  (iter (for range :in (row-for-merge thread))
	(setf (slot-value (ole sheet :range range) :MergeCells)
	      t)))

(defun shrink-to-fit (sheet lastrow)
  (setf (slot-value (ole sheet :range (format nil "J2:J~A" lastrow))
		    :ShrinkToFit)
	t)
  (setf (slot-value (ole sheet :range "A1:M1")
		    :ShrinkToFit)
	t))

;; printarea
;; orientation
;; printtitlerows
;; fittopagestall
;; fittopageswide
;; centerfooter
;; leftheader
;; rightheader
;; "&18&\"Palatino Linotype\"&P/&N"
;; "&\"HGP明朝B\"&36~A支部"
;; 
(defmacro page-setup (sheet &rest args)
  `(progn
     ,@(mapcar
	(lambda (l)
	  (optima:match l
	    ((LIST sym value)
	     `(setf (slot-value (ole ,sheet :PageSetup) ,sym) ,value))))
      (group args 2))))

;; (page-setup sh
;; 	    :PrintArea "A2:J200")

;; 100 受診する(種別不明) 101 支部健診を受診する 102 共通メニューを受診する 103 特定健診を受診する
;; 200 受診しない(理由不明) 201 多忙のため 202 職場の健診を受けた(る)ため 203 治療中 204 不便 205 健診制度の無理解
(defun xls (pathname &optional exception)
  (let* ((shibu    (create-shibu pathname exception))
	 (%thread% (sb-thread:make-thread
		    (lambda () (expose shibu)))))
    (with-excel (app :visible t :quit nil)
      (let* ((book (ole app :Workbooks :Add))
  	     (sh   (ole book :Worksheets :item 1)))
	(%PutData sh %thread%)
	(let ((lr (lastrow sh :x 1 :y 1)))
	  (excel::set-colwidth sh width)
	  (setf (slot-value (ole sh :range (format nil "A1:M~A" lr) :Borders)
			    :LineStyle)
		1)
	  (setf (slot-value (ole sh :range (format nil "A1:M~A" lr) :Borders)
			    :Weight)
		1)
	  (line-erase sh %thread%)
	  (line-merge sh %thread%)
	  (shrink-to-fit sh lr)
	  (set-fontname sh (:a 1) (:m lr) "ＭＳ Ｐ明朝")
	  (page-setup sh
		      :PrintArea	(format nil "A1:M~A" lr)
		      :Orientation	2
		      :PaperSize	12
		      :PrintTitleRows	"$1:$1"
		      :FittoPagesTall	nil
		      :FittoPagesWide	1
		      :RightHeader	"&9&\"Palatino Linotype\"対応コード…(10)受診する(種別不明) (11)支部健診受診 (12)共通メニュー受診 (13)特定健診受診 (20)受診しない(理由不明) (21)多忙のため (22)職場の健診を受ける(た)ため (23)治療中 (24)不便 (25)無理解 (31)不在 (32)ルス電"
		      :CenterFooter	"&18&\"Palatino Linotype\"&P/&N"
		      :RightFooter	(format nil "&\"HGP明朝B\"&28~A支部"
						(shibu-name shibu)))
 	  (ole sh :ResetAllPageBreaks)
	  (iter (for row :in (pagebreak %thread%))
		(print row)
	  	(setf (slot-value (ole sh :range (format nil "~A:~:*~A" row))
	  			  :PageBreak)
	  	      -4135)))))))
