(in-package :zenken)

(declaim (inline chomp create-zenken))

(defparameter %file ;#P"f:/20130628/特定健診全件データ.csv"
  (or (cl-fad:file-exists-p #P"d:/特定健診システム/特定健診CSV/特定健診全件データ.csv")
      (cl-fad:file-exists-p #P"f:/20130628/特定健診全件データ.csv")))
(defparameter %directory '(#P"d:/特定健診システム/特定健診CSV/"
		     #P"f:/20130628/"))

(defparameter years '(2008 2009 2010 2011 (2012 . "特定健診全件データ.csv") 2013))

(defmacro select (name)
  `(merge-pathnames (find-if #'file-exists-p %directory)
		    ,name))

(defun generate-files ()
  (iter (for y :in years)
	(if (atom y)
	    (collect (cons y (select (format nil "~A特定健診全件データ.csv" y))))
	    (collect (cons (car y) (select (cdr y)))))))

(defstruct (zenken
	     (:constructor zenken-gen
			   (年度末支部 名 保険証番号 行 氏名 本人／家族
性別 生年月日 年度末年齢 途中取得日
途中喪失日 除外 整理番号 発行日 受診日 受診区分
健診機関 取込方法 保健指導 指導日 支援
証支部 個人番号 組合番号 支部 分会 班 表示順))
	     (:constructor make-zenken (支部)))
  年度末支部 名 保険証番号 行 氏名 本人／家族 性別 生年月日 年度末年齢 途中取得日 途中喪失日
  除外 整理番号 発行日 受診日 受診区分 健診機関 取込方法 保健指導 指導日 支援 証支部 個人番号 組合番号
  支部 分会 班 表示順 上名 下名 分会名 現支部 健診機関コード)

(defun chomp (str)
  (labels ((in (subs)
	     (if (equal subs "")
		 ""
		 (if (or (char-equal (aref subs 0) #\ )
			 (char-equal (aref subs 0) #\　))
		     (in (subseq subs 1 (length subs)))
		     (reverse subs)))))
    (in (reverse str))))

(defparameter x (make-list 100000 :initial-element "20130714"))


(defmacro with-zenken-slot (instance &rest body)
  `(with-slots (年度末支部 名 保険証番号 行 氏名 本人／家族 性別 生年月日
			  年度末年齢 途中取得日 途中喪失日 除外 整理番号 発行日 受診日
			  健診機関 取込方法 保健指導 指導日 支援 証支部 個人番号 組合番号
			  支部 分会 班 表示順 上名 下名 分会名 現支部 健診機関コード) ,instance
     ,@body))

(defun solve-bunkai (kgbg shibu bunkai bhash)
  (cl-irregsexp:if-match-bind
   ((char) (char) "85" (+ (char)))
   (the string kgbg)
   (gethash (format nil "~2,'0d~2,'0d" shibu bunkai) bhash)
   (cl-irregsexp:if-match-bind
    ((integer :length 2) (s (integer :length 2)) (b (integer :length 2)) (+ (string)))
    (the string kgbg)
    (gethash (format nil "~2,'0d~2,'0d" s b) bhash))))

(defun create-zenken (list bhash)
  (declare (type list list) (type hash-table bhash)
	   (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
	   ;; (optimize (speed 0) (safety 3) (debug 3) (compilation-speed 0) (space 0))
	   )
  (let1 obj (apply #'zenken-gen list)
    (with-zenken-slot obj
      (setq 氏名	(chomp 氏名)
	    取込方法	(chomp 取込方法)
	    保健指導	(chomp 保健指導)
	    支援	(chomp 支援)
	    生年月日	(date-string-normalize (strdt 生年月日))
	    途中取得日	(date-string-normalize (strdt 途中取得日))
	    途中喪失日	(date-string-normalize (strdt 途中喪失日))
	    発行日	(date-string-normalize (strdt 発行日))
	    受診日	(date-string-normalize (strdt 受診日))
	    指導日	(date-string-normalize (strdt 指導日))
	    分会名	(solve-bunkai 保険証番号 支部 分会 bhash)
	    現支部	(if (equal "85" 証支部)
			    (subseq (the string 保険証番号) 4 6)
			    証支部)
	    健診機関コード 健診機関
	    健診機関	(or (car (gethash 健診機関 kensin:hospital-short-hash nil))
			    (second (gethash 健診機関 kensin:dock-hash nil))
			    ""))
      ;; (cl-irregsexp:if-match-bind
      ;;  ((f (+ (string))) (the string "　") (u (+ (string))))
      ;;  (the string 氏名)
      ;;  (setq 上名 (the string f)
      ;; 	     下名 (the string u)))
      (the zenken obj))))

(defun target? (obj)
  (declare (type zenken obj)
	   (optimize (speed 3) (safety 0) (debug 0))
	   ;; (optimize (speed 0) (safety 3) (debug 3))
	   )
  (optima:match obj
    ((zenken 年度末年齢 途中取得日 途中喪失日 除外)
     (let1 year (the integer (read-from-string 年度末年齢))
       (and (>= year 40) (< year 75)
	    (not 途中取得日) (not 途中喪失日)
	    (equal "0" 除外))))))

(defun to-data (filename)
  (let ((bhash (kensin:bunkai-hash)))
    (util::csv-read-filter-map
     filename
     (lambda (line)
       (handler-case (create-zenken line bhash)
	 (sb-int:simple-program-error (e)
	   (declare (ignorable e)) nil)))
     (lambda (z)
       (and (typep z 'zenken)
	    (not (equal (zenken-年度末支部 z) "年度末支部"))))
     :code :SJIS)))


(defun iterate (func &key (file %file))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type pathname file))
  (let ((bhash (kensin::bunkai-hash)))
    (util::csv-read-iter
     file
     (lambda (line)
       (optima:match line
	 ((list* "" _) nil)
	 ((list* "年度末支部" _) nil)
	 ;; ((list* _ _ _ _ _ _ _ _ _ _ _ _ "00000000000" _) nil)
	 (_
	  (funcall func (create-zenken line bhash)))))
       :code :SJIS)))

(defun filter-map (pred func &key (file %file))
  (let (r)
    (declare (dynamic-extent r))
    (iterate
      (lambda (zenken)
	(if (funcall pred zenken)
	    (setq r (cons (funcall func zenken) r))))
      :file file)
    r))

(defun shibu-list
    (shibu-code &key (file %file) (function #'identity) (filter #'identity))
  (filter-map
   (lambda (z) (and (string= shibu-code (zenken-支部 z))
		    (funcall filter z)))
   function
   :file file))

(defmacro defhash (defname keyfn)
  `(defun ,defname (filename &key (verbose t))
     (iter (with list    = (to-data filename))
	   (for  line    :in list)
	   (phash line :condition t
		       :key ,keyfn))))

(defhash make-hash	    (lambda (o) (normal-date->string (strdt (zenken-生年月日 o)))))
(defhash make-name-hash     #'zenken-氏名)

(defun make-jusinken-hash (file)
  (let ((hash (make-hash-table :test #'equal)))
    (iterate
      (lambda (zenken)
	(with-slots (整理番号) zenken
	  (setf (gethash 整理番号 hash) zenken)))
      :file file)
    hash))

(defun make-jusinken-simple-hash (filename)
  (iter (for line :in (to-data filename))
	(shash line
	       :value     #'zenken-整理番号
	       :key       #'zenken-個人番号
	       :condition t;; (>= (read-from-string (zenken-年度末年齢 line)) 40)
	       )))

(defun id-hash (filename basedate)
  (iter (for line :in (to-data filename))
	(with-zenken-slot line
	  ;; (if (and (>= (how-old 生年月日 basedate) 40)
	  ;; 	   (<= (how-old 生年月日 basedate) 75))
	  ;;     (shash line
	  ;; 	     :condition t
	  ;; 	     :key   #'zenken-個人番号
	  ;; 	     :value (lambda (o)
	  ;; 		      (with-zenken-slot o
	  ;; 			(list 氏名 受診日
	  ;; 			      (if (equal 健診機関コード "0000000000") nil 健診機関コード)
	  ;; 			      (if (string-null 健診機関) nil 健診機関)
	  ;; 			      (if (string-null 取込方法) nil 取込方法)
	  ;; 			      発行日 分会 班 分会名 現支部
	  ;; 			      年度末支部 整理番号)))))
	  (shash line
	  	 :condition t
	  	 :key   #'zenken-個人番号
	  	 :value (lambda (o)
	  		  (with-zenken-slot o
	  		    (list 氏名 受診日
	  			  (if (equal 健診機関コード "0000000000") nil 健診機関コード)
	  			  (if (string-null 健診機関) nil 健診機関)
	  			  (if (string-null 取込方法) nil 取込方法)
	  			  発行日 分会 班 分会名 現支部
	  			  年度末支部 整理番号))))
	  )))

(defun id-hash2 (filename basedate)
  (iter (for line :in (to-data filename))
	(with-zenken-slot line
	  ;; (if (and (>= (how-old 生年月日 basedate) 40)
	  ;; 	   (<= (how-old 生年月日 basedate) 75))
	  ;;     (shash line
	  ;; 	     :condition t
	  ;; 	     :key   #'zenken-個人番号
	  ;; 	     :value (lambda (o)
	  ;; 		      (with-zenken-slot o
	  ;; 			(list 氏名 受診日
	  ;; 			      (if (equal 健診機関コード "0000000000") nil 健診機関コード)
	  ;; 			      (if (string-null 健診機関) nil 健診機関)
	  ;; 			      (if (string-null 取込方法) nil 取込方法)
	  ;; 			      発行日 分会 班 分会名 現支部
	  ;; 			      年度末支部 整理番号)))))
	  (shash line
	  	 :condition t
	  	 :key   (lambda (o)
			  (format nil "0~A" (zenken-個人番号 o)))
	  	 :value (lambda (o)
	  		  (with-zenken-slot o
	  		    (list 途中取得日 途中喪失日))))
	  )))

(defun id-hash3 ()
  (let ((hash (make-hash-table :test #'equal)))
    (iterate
     (lambda (z) (setf (gethash (zenken-個人番号 z) hash) z))
     :file %file)
    hash))

(defun complex-hash2 (filename)
  (iter (for line :in (to-data filename))
	(phash line
	       :condition t
	       :key   (lambda (o) (with-zenken-slot o (list 氏名 生年月日)))
	       :value (lambda (o)
			(with-zenken-slot o
			  (list 整理番号 保険証番号 生年月日 氏名 途中取得日 途中喪失日
				受診日 健診機関 支部 分会 班 分会名 現支部))))))

(defun hash () (make-hash %file))

(defmacro zfind (test &rest body)
  `(iter (for zenken :in (to-data %file))
	 (if (with-zenken-slot zenken ,test)
	     (with-zenken-slot zenken
	       (progn
		 ,@body)))))

(defun string-space-filled? (string)
  (declare (type simple-string string)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((in (subs)
	     (if (string-null subs)
		 t
		 (let1 head (the character (aref subs 0))
		   (if (or (char-equal head #\ )
			   (char-equal head #\　))
		       (in (the string (subseq subs 1 (length subs))))
		       nil)))))
    (if (string-null string)
	nil
	(in string))))

(declaim (inline kensin:kensin-year? string-space-filled?))

(defun month-calc (file)
  (iter (with array = (make-array '(96 13) :initial-element nil))
	(for line :in-csv file :code :SJIS)
	(if (first-time-p) (next-iteration))
	(optima:match line
	  ((list* shibu _ _ _ _ _ _ _ o g r _ _ _ date hospital _)
	   (if (and (kensin:kensin-year? (read-from-string o))
		    (string-space-filled? g)
		    (string-space-filled? r)
		    (not (equal hospital "0000000000")))
	       (let* ((m     (date-month (strdt date)))
		      (month (if (< m 4) (+ m 9) (- m 4))))
		 (setf (aref array (read-from-string shibu) month)
		       (cons hospital (aref array (read-from-string shibu) month)))))))
	(finally (return array))))

(defpackage :zenken-calc
  (:nicknames #:zc)
  (:use #:cl #:util #:kensin #:iterate #:zenken)
  (:import-from #:zenken #:to-data)
  (:import-from #:optima #:match)
  (:import-from #:alexandria
		#:hash-table-keys
		#:hash-table-values
		#:hash-table-alist
		#:alist-hash-table))

(in-package #:zenken-calc)

(defvar file ksetting::*zenken-file*)
(defvar vecfile ksetting::*zenken-vec-file*)

(declaim (inline string->code translate))

(defun string->code (str)
  (match str
    ((or "本人" "男") 1)
    ((or "家族" "女") 2)
    ("0000000000"     nil)
    (_ t)))

(defun translate (line number)
  (match number
    ((or 1 9 24 25) (read-from-string (nth (1- number) line)))
    ((or 6 7 16)    (string->code (nth (1- number) line)))))

(defstruct bunkai shibu-code shibu-name code name h k m f total)

(defun make-bunkai-array ()
  (make-array '(2 76) :initial-element 0))

(defun make-bunkai-hash ()
  (iter (with hash = (make-hash-table :test #'eq))
	(for (k v) :in-hashtable (kensin::bunkai-hash))
	(for shibu = (string-take k 2))
	(setf (gethash (read-from-string k) hash)
	      (make-bunkai :shibu-code shibu
			   :shibu-name (long-shibu shibu)
			   :code k
			   :name v
			   :h (make-bunkai-array)
			   :k (make-bunkai-array)
			   :m (make-bunkai-array)
			   :f (make-bunkai-array)
			   :total (make-bunkai-array)))
	(finally (return hash))))

(defmacro bunkai+ (bunkai func hit year &key (total t))
  `(progn
     (when ,hit
       (aref-1+ (,func ,bunkai) 0 ,year)
       ,(if total
	    `(aref-1+ (bunkai-total ,bunkai) 0 ,year)))
     (aref-1+ (,func ,bunkai) 1 year)
     ,(if total
	  `(aref-1+ (bunkai-total ,bunkai) 1 ,year))))

(defun bunkai-h+ (bunkai hit year)
  (bunkai+ bunkai bunkai-h hit year))
(defun bunkai-k+ (bunkai hit year)
  (bunkai+ bunkai bunkai-k hit year))
(defun bunkai-m+ (bunkai hit year)
  (bunkai+ bunkai bunkai-m hit year :total nil))
(defun bunkai-f+ (bunkai hit year)
  (bunkai+ bunkai bunkai-f hit year :total nil))

(defun to-vector (line)
  (map 'vector
       (lambda (n) (translate line n))
       ;; hit shibu hx sex year-old shibu bunkai
       (list 16 1 6 7 9 24 25)))

(defun to-array (file)
  (iter (with hash = (make-bunkai-hash))
	(for line :in-csv file :code :SJIS)
	(match line
	  ((LIST* "年度末支部" _) (next-iteration))
	  ((LIST  "") (next-iteration))
	  (_
	   (match (to-vector line)
	     ((VECTOR hit shibu hk sex year-old _ bunkai)
	      (aif (gethash (+ (* 100 shibu) bunkai) hash)
		   (progn
		     (match hk
		       (1 (bunkai-h+ it hit year-old))
		       (2 (bunkai-k+ it hit year-old)))
		     (match sex
		       (1 (bunkai-m+ it hit year-old))
		       (2 (bunkai-f+ it hit year-old))))
		   (next-iteration)))
	     ((TYPE NULL)
	      (next-iteration)))))
	(finally (return hash))))

(defun vec-read ()
  (alist-hash-table
   (call-with-input-file2 vecfile #'read)
   :test #'eq))

(defun bunkais ()
  (sort2 (mapcar #'read-from-string
		 (remove-if
		  (lambda (k) (or (ppcre:scan "^90" k)
				  (ppcre:scan "^95" k)))
		  (hash-table-keys (kensin::bunkai-hash))))
	 < identity))

;; 2次元配列を開始位置から終了位置を指定して抜き出す。
(defun row-major-aref-list (array from to &key (plus 0))
  (mapcar (lambda (n) (row-major-aref array (+ plus n)))
	  (iota :from from :to to)))

(defun row-major-aref-sum (array from to &key (plus 0))
  (apply #'+ (row-major-aref-list array from to :plus plus)))

(defparameter yearlist '((0 19) (20 29) (30 39) (40 49) (50 59) (60 69) (70 75)))

(defun figure (bunkai plus)
  (with-slots (h k m f total) bunkai
    (iter (for l :in (list total h k m f))
	  (collect
	      (util::append-total1	;; 列ごとの合計をとる
	       (iter (for (start end) :in yearlist)
		     (collect (row-major-aref-sum l start end :plus plus))))))))

(defun percent-or-nil (num div)
  (if (eq div 0)
      "-"
      (util::percent num div)))

(defun take40-75 (list)
  (butlast (util::take-right list 5)))

(defun sum40-75 (list)
  (apply #'+ (take40-75 list)))

(defun figure0 (bunkai)
  (let ((l1 (figure1 bunkai))
	(l2 (figure2 bunkai)))
    (with-slots (shibu-name name) bunkai
      (mapcar
       (lambda (x1 x2) (append (list shibu-name name)
			       x2
			       x1
			       (mapcar #'percent-or-nil x2 x1)
			       (let ((sum-over-40 (sum40-75 x2))
				     (sum-over-40-whole (sum40-75 x1)))
				 (list sum-over-40
				       sum-over-40-whole
				       (percent-or-nil sum-over-40 sum-over-40-whole)))))
       l1 l2))))
(defun figure1 (bunkai) (figure bunkai 76))
(defun figure2 (bunkai) (figure bunkai 0))

(defun %calc ()
  (let ((hash (vec-read)))
    (mapcan (lambda (n) (figure0 (gethash n hash)))
	    (bunkais))))
  
(in-package :zenken)

(defun vec-output ()
  (call-with-output-file2 zc::vecfile
    (lambda (op)
      (write (alexandria:hash-table-alist
	      (zc::to-array zc::file))
	     :stream op))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defparameter x (cl-store:restore #P"f:/zenken.csv"))

;; (defun _f (date)
;;   (util::date-8 date))

;; (defun hot2 ()
;;   (iter (with hash = (hot1))
;; 	(for line
;; 	  :in (cdr (call-with-input-file2 "ito" #'read)))
;; 	(optima:match line
;; 	  ((LIST* nil _)
;; 	   (next-iteration))
;; 	  ((LIST* d _ _ _ _ birth sex _)
;; 	   (let ((key (format nil "~A~A~A" (truncate d) birth sex)))
;; 	     (aif (gethash key hash)
;; 		  (next-iteration)
;; 		  (collect line)))))))

;; (iter (with hash = (make-hash-table :test #'equal))
;;       (for line :in (hot2))
;;       (aif (gethash line hash)
;; 	   (error line)
;; 	   (setf (gethash line hash) line)))

;; (defun hot1 ()
;;   (iter (with hash = (make-hash-table :test #'equal))
;; 	(for line :in (to-data ksetting::*zenken-file*))
;; 	(with-slots (年度末支部 支部 氏名 生年月日 受診日 健診機関 性別) line
;; 	  (if (and (equal "10" 年度末支部)
;; 		   (equal "上京" 健診機関))
;; 	      (setf (gethash (format nil "~A~A~A"
;; 				     (util::date-8 受診日)
;; 				     (util::date-8 生年月日)
;; 				     性別)
;; 			     hash)
;; 		    1)))
;; 	(finally (return hash))))

;; (defun ho3 ()
;;   (with-excel (ap :visible t :quit nil)
;;     (with-excel-book (ap bk "f:/20131025/伊東.xlsx" :close nil)
;;       (let* ((sh (ole bk :WorkSheets :Item 1))
;; 	     (uv (slot-value (ole sh :UsedRange) :Value)))
;; 	(iter (for line :in uv)
;; 	      (optima:match (car line)
;; 		((TYPE FLOAT)
;; 		 )))))))

(defun calc ()
  (zc::%calc))

;; 受診していないが、受診券を発行したもの
(defun dock-diff (&key (filter #'identity))
  (let ((hash (make-jusinken-hash %file))
	r)
    (util::csv-read-iter
     ksetting::*dock-output-file*
     (lambda (line)
       (let ((jnum (sixth line)))
	 (if (and (eq (length jnum) 11)				; 40歳未満75歳より上を排除
		  (eq (read-from-string (string-take jnum 2))	; 設定年度以外を排除
		      (mod ksetting::*year* 1000)))
	     (aif (gethash jnum hash)
		  (if (and (target? it)
			   ;; 受診記録がない。->受診記録があれば、全件データでわかるため。
			   (not (zenken-受診日 it))
			   ;; 受診券発行記録がある。
			   (ZENKEN-発行日 IT)
			   (funcall filter it))
		      (setq r (cons it r))))))))
    (REVERSE R)))

(defun dock-diff-shibu (shibu-code)
  (dock-diff
   :filter (lambda (z) (string= shibu-code (zenken-支部 z)))))

(defun dock-diff-shibu-number (shibu-code)
  (mapcar (lambda (z) (list (zenken-保険証番号 z) (zenken-整理番号 z)))
	  (dock-diff-shibu shibu-code)))

(in-package :cl-user)
