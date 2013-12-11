(in-package :kensin) ;----------------------------------------------------------------------

(defun 172-newest ()
  (or (cl-fad:file-exists-p #P"d:/zipfile/172csv/FKCA172.csv")
      (cl-fad:file-exists-p #P"f:/FKCA172.csv")
      (cl-fad:file-exists-p #P"y:/47伊東/FKCA172.csv")))

(defun int (string)
  (read-from-string string))

(defstruct (172data
	     (:constructor 172-gen (保険者番号 被保険者証記号 被保険者証番号 個人番号 データ管理番号１ 性別 生年月日 被保険者名カナ 被保険者名漢字 資格フラグ 除外フラグ 受診券整理番号 保健指導レベル 服薬再確認 健診メッセージID 健診メッセージ 利用券整理番号 初回面接実施日 途中終了 指導未完了 指導メッセージID 指導メッセージ))
	     ;; (:constructor )
	     )
  保険者番号 被保険者証記号 被保険者証番号 個人番号 データ管理番号１ 性別 生年月日 被保険者名カナ 被保険者名漢字 資格フラグ 除外フラグ 受診券整理番号 保健指導レベル 服薬再確認 健診メッセージID 健診メッセージ 利用券整理番号 初回面接実施日 途中終了 指導未完了 指導メッセージID 指導メッセージ 支部)


(defun create-172data (list)
  (let1 obj (apply #'172-gen list)
    (with-slots (生年月日 支部 被保険者証記号 被保険者証番号) obj
      (setq 生年月日	(normal-date->string (strdt 生年月日))
	    支部	(cl-irregsexp:if-match-bind
			 ("建" (nendo (string 2)) (skanji (string)) (shibu (string 2)))
			 被保険者証記号
			 (if (equal shibu "８５")
			     (cl-irregsexp:if-match-bind
			      ((scode (string 2)) _) 被保険者証番号
			      (to-hankaku scode))
			     (to-hankaku shibu))))
      obj)))

(defun 172-mother ()
  (mapcar
   #'create-172data
   (nthcdr 2 (csv-read-to-list (172-newest) :code :SJIS))))

(defun 172-create-mother (filename)
  (mapcar
   #'create-172data
   (nthcdr 2 (csv-read-to-list filename :code :SJIS))))

(defun 172-2011-mother ()
  (mapcar
   #'create-172data
   (nthcdr 2 (csv-read-to-list #P"f:/FKAC172_2011.csv" :code :SJIS))))

(defun 172-newest-date ()
  (call-with-input-file2 "y:/47伊東/FKCA172.csv"
    (lambda (op)
      (third (ppcre:split "," (read-line op nil nil nil))))
    :code :SJIS))

(defun 172-hash ()
  (push-hash-table
   #'172data-受診券整理番号
   #'identity
   (172-mother)))

(defun 172-make-hash (filename)
  (push-hash-table
   #'172data-受診券整理番号
   #'identity
   (172-create-mother filename)))

(defun 172-target? (obj)
  (declare (type 172data obj)
	   (optimize speed))
  (with-slots (健診メッセージ 指導メッセージID 受診券整理番号) obj
    (and (string-null 健診メッセージ)
	 (string-not-null 受診券整理番号)
	 (not (equal 指導メッセージID "MKCA01746E")))))

(defun 172-2011-hash ()
  (push-hash-table
   #'172data-受診券整理番号
   #'identity
   (172-2011-mother)))

(defun 172-complex-hash ()
  (push-hash-table
   (lambda (o)
     (with-slots (被保険者名漢字 生年月日) o
       (list 被保険者名漢字 生年月日)))
   #'identity
   (172-mother)))

(defun 172-hash2 ()
  (push-hash-table
   #'172data-生年月日
   #'identity
   (172-mother)))

(defun 172-total ()
  (iter (for line :in-csv (172-newest) :code :SJIS)
	(if (first-time-p) (next-iteration))
	(for instance = (create-172data line))
	(if (and (string-null (172data-健診メッセージ instance))
		 (string-null (172data-指導メッセージ instance)))
	    (chash instance :key #'172data-支部))))

(defun 172-hsido-classify ()
  (iter (for person :in (172-mother))
	(phash person
	       :condition t
	       :key #'172data-保健指導レベル)))

(defun 172-hsido-total-user (list)
  (iter (for person :in list)
	(if (and (string-not-null (172data-指導未完了 person))
		 (not (equal "MKCA01746E" (172data-指導メッセージID person))))
	    (collect person))))

(defun 172-hsido-total-finisher (list)
  (iter (for person :in list)
	(if (and (equal "0" (172data-指導未完了 person))
		 (not (equal "MKCA01746E" (172data-指導メッセージID person))))
	    (collect person))))

(defun percent (num divisor &key (power 1))
  (float (/ (round (* (expt 10 power) (* 100 (float (/ num divisor)))))
	    (expt 10 power))))

;; (* 100 (float (/ 23 839)))

(defun 172-hsido-total-output-csv (plist)
  (let* ((l1	(getf plist :level1))
	 (l1u	(getf plist :level1-user))
	 (l1f	(getf plist :level1-finisher))
	 (l2	(getf plist :level2))
	 (l2u	(getf plist :level2-user))
	 (l2f	(getf plist :level2-finisher)))
    (format nil "~{~A~^,~}"
	    (list l2 l2u (percent l2u l2) l2f (percent l2f l2)
		  l1 l1u (percent l1u l1) l1f (percent l1f l1)
		  (+ l2 l1) (+ l1u l2u) (percent (+ l2u l1u) (+ l2 l1))
		  (+ l1f l2f) (percent (+ l2f l1f) (+ l2 l1))))))

(defun 172-hsido-total (&optional (output-type :identity))
  "output-type  :identity :csv :xml :yaml"
  (let* ((mainhash (172-hsido-classify))
	 (level1   (gethash "1" mainhash))
	 (level2   (gethash "2" mainhash)))
    (funcall
     (case output-type
       (:identity	#'identity)
       (:csv		#'172-hsido-total-output-csv))
     (list :level1          (length level1)
	   :level1-user     (length (172-hsido-total-user level1))
	   :level1-finisher (length (172-hsido-total-finisher level1))
	   :level2          (length level2)
	   :level2-user     (length (172-hsido-total-user level2))
	   :level2-finisher (length (172-hsido-total-finisher level2))))))

(defun 172-uploaded? (jnumber hash)
  (optima:match (gethash jnumber hash nil)
    ((LIST 172data)
     (optima:match 172data
       ((172data kensin::健診メッセージ kensin::指導メッセージ)
	(if (and (util:string-null kensin::健診メッセージ)
		 (util:string-null kensin::指導メッセージ))
	    :uploaded
	    (format nil "~A~A" kensin::健診メッセージ kensin::指導メッセージ)))))
    (nil :not-uploaded)))

(defun how-old2 (birth number)
  (cl-irregsexp:if-match-bind
   ((year (integer :length 2)) _) (the string number)
   (how-old birth (nendo-end (+ 2000 year)))))

(defmacro-driver (FOR var IN-SHIBU type)
  `(progn (for ,var :in (case ,type
			  (:long  long-shibu-alist)
			  (:short short-shibu-alist)))
	  ;; (if (equal ,(car var) "85") (next-iteration))
	  ,(optima:match var
	    ((cons x y)
	     `(if (equal ,x "85") (next-iteration)))
	    (x
	     `(if (equal (car ,x) "85") (next-iteration))))))

(defmacro defalias (name f)
  `(defun ,name (&rest args)
     (funcall ,f args)))

(defpackage #:r172-totalize
  (:nicknames :r172t)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:export #:classify
	   #:collect-title
	   #:init-list
	   #:percent-or-nil
	   #:generate-merge-cells))

(in-package #:r172t) ;----------------------------------------------------------------------

(defun make-array2 ()
  (make-array '(96 3 76) :initial-element 0))

(defun make-array3 ()
  (make-array '(96 5) :initial-element nil))

(defun classify (csvdata)
  (iter (with sex-year-array = (make-array2))
	(with hkarray        = (make-array2))
	(with hlv            = (make-hash-table :test #'equal))
	(with sidoary        = (make-array3))
	(for line :in csvdata)
	(for obj = (kensin::create-172data line))
	(with-slots (kensin::指導メッセージ
		     kensin::資格フラグ kensin::支部 kensin::性別
		     kensin::生年月日 kensin::受診券整理番号 kensin::保健指導レベル)
	    obj
	  (optima:match (list kensin::指導メッセージ kensin::資格フラグ)
	    ((LIST mes _)
	     (if (string-not-null mes)
		 (progn
		   (collect obj :into sido)
		   (next-iteration))
		 (optima:fail)))
	    ((LIST _ "1")
	     (collect obj :into kensin1)
	     (next-iteration))
	    ((LIST _ "2")
	     (collect obj :into kensin2)
	     (next-iteration)))
	  (let ((shibu (kensin::int kensin::支部)) (sex (kensin::int kensin::性別))
		(hk (if (ppcre:scan "1$" kensin::受診券整理番号) 1 2))
		(year  (jnum-how-old kensin::生年月日 kensin::受診券整理番号)))
	    (aref-1+   sex-year-array shibu sex year)
	    (aref-1+   hkarray shibu hk year)
	    (aref-cons sidoary (list kensin::受診券整理番号 year)
		       shibu (kensin::int kensin::保健指導レベル))
	    (push-hash kensin::支部 kensin::保健指導レベル hlv)))
	(finally (return (values sido kensin1 kensin2 sex-year-array hkarray hlv sidoary)))))

(defun get-by-sex (array shibu sex year &key (step 5))
  (iter (for y :from year :to (min 75 (+ year (1- step))))
	(sum (aref array shibu sex y))))

(defun collect-title (step &key (seed '("男性" "女性")))
  (optima:match seed
    ((list _ _)
     (iter (for y :in (iota :from 40 :to 74 :step step))
	   (appending (mapcar (lambda (s) (format nil "~A-~A~%~A" y (min 74 (1- (+ step y))) s))
			      seed))))
    (_ (error "(x y) is required."))))

(defun init (ary c step)
  (iter (for year :in (iota :from 40 :to 74 :step step))
	(appending (iter (for sex :in '(1 2))
			 (collect (get-by-sex ary c sex year :step step) :into pot)
			 (finally (return pot))))))

(defun init-list (ary step)
  (iter (for (code . name) :in-shibu :long)
	(for c = (kensin::int code))
	(for init = (init ary c step))
  	(collect (append (list code name)
  			 init
  			 (mapcar (lambda (f) (apply #'+ (mapcar f (group init 2))))
  				 (list #'first #'second))
  			 (list (apply #'+ init))))))

(defun percent-or-nil (num div)
  (if (eq div 0)
      "-"
      (util::percent num div)))

(defun generate-merge-cells (step)
  (iter (for i :from 2 :to (+ 2 (* 26 step)) :by step)
	(appending (list (format nil "A~A:A~A" i (+ (1- step) i))
			 (format nil "B~A:B~A" i (+ (1- step) i))))))

(defclass SHEET ()
  ((book         :initarg :book)
   (sheet        :initarg :sheet :accessor sheet-of)
   (name         :initarg :name)
   (borders-area :initarg :borders-area)
   (width        :initarg :width)
   (jpfont       :initarg :jpfont)
   (enfont       :initarg :enfont)
   (c-align-area :initarg :c-align-area)
   (title        :initarg :title)
   (endcol       :initarg :endcol)
   (end          :initarg :end :reader end-of)
   (shibu-step   :initarg :shibu-step)
   (shibu-row    :initarg :shibu-row)
   (shibu-col	 :initarg :shibu-col)))

(defmethod initialize-instance :after ((m SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (book name step) m
    (let1 sh (ole book :worksheets :add)
      (excel::sheet-name sh name)
      (setf (sheet-of m) sh))))

(defpackage #:r172-type1
  (:nicknames :r1721)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel :r172t))

(in-package :r1721) ;----------------------------------------------------------------------

(defun dock-shibu (bango)
  (cl-irregsexp:if-match-bind
   ((shibu (integer :length 2)) (bunkai (integer :length 2)))
   (the simple-string bango)
   (if (eq shibu 85) bunkai shibu)))
  
(defun dock-target? (occur number hash)
  (and (equal (nendo-year occur) ksetting::*year*)
       (aif (gethash number hash)
	    (zenken::target? it) nil)))

(defun dock-read (hash)
  (iter (for line :in (csv-read-to-list ksetting::*dock-output-file*))
	(optima:match line
	  ((list* _ _ kgbg _ birth number "1" _ _ _ occ _)
	   (if (dock-target? occ number hash)
	       (collect (vector (dock-shibu kgbg)
				(jnum->sex number hash)
				(hk number)
				(jnum-how-old birth number))))))))

(defun sc-read (hash)
  (iter (for line :in (csv-read-to-list ksetting::*sc-output-file*))
	(optima:match line
	  ((list* num y _ _ _ _ shibu _)
	   (when (eq ksetting::*year* (+ 2000 (kensin::int y)))
	     (let1 obj (gethash num hash)
	       (if (aif obj (zenken::target? it) nil)
		   (collect
		       (vector (kensin::int (gethash shibu short-vice-shibu-hash))
			       (sex (zenken::zenken-性別 obj))
			       (hk num)
			       (kensin::int (zenken::zenken-年度末年齢 obj)))))))))))

(defun sex-array (list)
  (iter (with ary = (make-array '(96 3 76)))
	(for line :in list)
	(optima:match line
	  ((vector shibu sex _ old) (aref-1+ ary shibu sex old)))
	(finally (return ary))))

(defun hk-array (list)
  (iter (with ary = (make-array '(96 3 76)))
	(for line :in list)
	(optima:match line
	  ((vector shibu _ hk old) (aref-1+ ary shibu hk old)))
	(finally (return ary))))

;; ("10" "北" 26 8 34 21 7 28 15 6 21 12 4 16 20 8 28 18 15 33 14 5 19 126 53 179)
(defun percent-calc (list)
  (destructuring-bind (code shibu . data) list
    (let1 last (last1 list)
      `(,code ,shibu
	      ,@(reduce (lambda (el seed) (cons (util::percent el last) seed))
			data
			:initial-value nil
			:from-end t)))))

(defun init (ary step)
  (append-total (init-list ary step)
		:from 2))

(defun percent (ary step)
  (mapcar #'percent-calc (init ary step)))

(defclass SHEET (R172T::SHEET)
  ((data1	 :initarg :data1)
   (data2	 :initarg :data2)
   (step         :initarg :step     :initform 5)
   (category     :initarg :category)
   (mainarray    :initarg :mainarray :reader array-of)))

(defmethod initialize-instance :after ((m SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (r172t::width r172t::borders-area r172t::jpfont r172t::enfont
			    r172t::c-align-area r172t::step r172t::title
			    data1 data2 category r172t::endcol r172t::end) m
    (setq r172t::title	`(("支部CD" "支部" ,@(r172t::collect-title step) ,@category "合計"))
    	  data1		(init (array-of m) step)
    	  data2		(percent (array-of m) step)
    	  r172t::endcol	(length (car r172t::title))
    	  r172t::end	(excel::number-to-col r172t::endcol)
    	  r172t::borders-area
	  `(,(format nil "A1:~A28" r172t::end)
	     ,(format nil "A30:~A57" r172t::end))
    	  r172t::c-align-area
	  `("A1:B28"
	    ,(format nil "A1:~A1" r172t::end)
	    "A30:B57"
	    ,(format nil "A30:~A57" r172t::end))
    	  r172t::enfont	`(,(format nil "C2:~A28" r172t::end)
    	  		  ,(format nil "C30:~A57" r172t::end)))
    m))
(defpackage #:r172-type2
  (:nicknames #:r1722)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel :r172t)
  (:import-from #:alexandria
		#:hash-table-values))

(in-package #:r1722); ----------------------------------------
(declaim (inline add))

(defstruct unit h k m f total)

(defun unit+ (unit1 unit2)
  (macrolet ((vsum (func)
	       `(vector-sum (,func unit1) (,func unit2))))
    (make-unit :h     (vsum unit-h)
	       :k     (vsum unit-k)
	       :m     (vsum unit-m)
	       :f     (vsum unit-f)
	       :total (vsum unit-total))))

(defun create-unit (size)
  (make-unit :h (make-array (list size) :initial-element 0)
	     :k (make-array (list size) :initial-element 0)
	     :m (make-array (list size) :initial-element 0)
	     :f (make-array (list size) :initial-element 0)
	     :total (make-array (list size) :initial-element 0)))

(defun add (unit hk sex script)
  (with-slots (h k m f total) unit
    (let ((vhk  (svref (vector 0 h k) hk))
	  (vsex (svref (vector 0 m f) sex))
	  (len  (1- (length h))))
      (aref-1+ vhk (1- script))
      (aref-1+ vsex (1- script))
      (aref-1+ total (1- script))
      (aref-1+ vhk len)
      (aref-1+ vsex len)
      (aref-1+ total len))))

(defun dock-read ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in (csv-read-to-list ksetting::*dock-output-file*))
	(optima:match line
	  ((list* _ _ _ _ _ number "1" _)
	   (setf (gethash number hash) 1)))
	(finally (return hash))))

(defun sc-read ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in (csv-read-to-list ksetting::*sc-output-file*))
	(optima:match line
	  ((list* num _)
	   (setf (gethash num hash) 1)))
	(finally (return hash))))

(defun unit-last (unit)
  (with-slots (h k m f total) unit
    (optima:match h
      ((TYPE VECTOR)
       (let ((len (1- (length h))))
	 (mapcar (lambda (v) (svref v len))
		 (list total h k m f))))
      ((TYPE FIXNUM)
       (list total h k m f)))))

(defun %list-expand (list)
  (optima:match list
    ((LIST k d s _t)
     (list k (percent-or-nil k _t)
	   d (percent-or-nil d _t)
	   s (percent-or-nil s _t)
	   _t))))

(defun make-spec (total dock sc)
  (let ((_total (unit-last total))
	(_dock  (unit-last dock))
	(_sc    (unit-last sc)))
    (iter (for i :from 0 :to 4)
	  (for _t = (nth i _total))
	  (for _d = (nth i _dock))
	  (for _s = (nth i _sc))
	  (for _k = (- _t _d _s))
	  (collect (%list-expand (list _k _d _s _t))))))

(defun make-spec2 (total dock sc)
  (let ((_total (unit-last total))
	(_dock  (unit-last dock))
	(_sc    (unit-last sc)))
    (iter (for i :from 0 :to 4)
	  (for _t = (nth i _total))
	  (for _d = (nth i _dock))
	  (for _s = (nth i _sc))
	  (for _k = (- _t _d _s))
	  (collect (list _k _d _s _t)))))

(def-clojure shibu (code name)
  ((code	code)
   (name	name)
   (hsido	(create-unit 5))
   (167m	(create-unit 13))
   (metabo	(create-unit 5))
   (dock	(make-unit :h 0 :k 0 :m 0 :f 0 :total 0))
   (sc		(make-unit :h 0 :k 0 :m 0 :f 0 :total 0)))
  (:add   (hk sex month hlv mlv)
	  (add 167m   hk sex month)
	  (add hsido  hk sex hlv)
	  (add metabo hk sex mlv))
  (:dock! (hk sex)
	  (if (eq hk 1)
	      (setf (unit-h dock) (1+ (unit-h dock)))
	      (setf (unit-k dock) (1+ (unit-k dock))))
	  (if (eq sex 1)
	      (setf (unit-m dock) (1+ (unit-m dock)))
	      (setf (unit-f dock) (1+ (unit-f dock))))
	  (setf (unit-total dock) (1+ (unit-total dock))))
  (:sc!   (hk sex)
	  (if (eq hk 1)
	      (setf (unit-h sc) (1+ (unit-h sc)))
	      (setf (unit-k sc) (1+ (unit-k sc))))
	  (if (eq sex 1)
	      (setf (unit-m sc) (1+ (unit-m sc)))
	      (setf (unit-f sc) (1+ (unit-f sc))))
	  (setf (unit-total sc) (1+ (unit-total sc))))
  (:spec  ()
	  (make-spec hsido dock sc))
  (:spec2 ()
	  (make-spec2 hsido dock sc)))

(defun generate-shibu ()
  (iter (with hash = (make-hash-table :test #'eq))
	(for (code . shibu) :in-shibu :long)
	(for c = (read-from-string code))
	(setf (gethash c hash)
	      (shibu c shibu))
	(finally (return hash))))

(defun %classify (172data 167hash)
  (iter (with shibu-hash = (generate-shibu))
	(with dock-hash  = (dock-read))
	(with sc-hash    = (sc-read))
	(for line :in 172data)
	(with-slots (kensin::支部
		     kensin::性別
		     kensin::受診券整理番号
		     kensin::保健指導レベル) line
	  (for jnum  = kensin::受診券整理番号)
	  (for shibu = (read-from-string kensin::支部))
	  (for hlv   = (read-from-string kensin::保健指導レベル))
	  (for sex   = (read-from-string kensin::性別))
	  (for hk    = (hk jnum))
	  (for r167  = (gethash jnum 167hash))
	  ;; (assert (typep r167 'kensin::r167))
	  (unless (typep r167 'kensin::r167)
	    (print jnum))
	  (for %m    = (kensin::r167-実施月 r167))
	  (for month = (if (> %m 3) (- %m 3) (+ %m 9)))
	  (for mlv   = (car (kensin::r167-メタボレベル r167)))
	  (for %S%   = (gethash shibu shibu-hash))
	  (funcall %S% :add hk sex month hlv mlv)
	  (cond
	    ((gethash jnum dock-hash)
	     (funcall %S% :dock! hk sex))
	    ((gethash jnum sc-hash)
	     (funcall %S% :sc! hk sex))
	    (t
	     (next-iteration))))
	(finally (return shibu-hash))))

(defun figure-unit (unit)
  (with-slots (h k m f total) unit
    (mapcar (lambda (v) (coerce v 'list))
	    (list total h k m f))))

(defun figure5-unit (unit)
  (with-slots (h k m f total) unit
    (iter (for v :in (list total h k m f))
	  (optima:match v
	    ((VECTOR lv1 lv2 lv3 ot total)
	     (collect (list lv1 (percent-or-nil lv1 total)
			    lv2 (percent-or-nil lv2 total)
			    lv3 (percent-or-nil lv3 total)
			    ot (percent-or-nil ot total)
			    total)))))))

(defun %type (hash sym)
  (mapcar (lambda (f) (funcall f sym))
	  (hash-table-values hash)))

(defun %total-type (hash sym)
  (reduce (lambda (x y)
	    (if x (unit+ x y) y))
	  (%type hash sym)
	  :initial-value nil))

(defun %total (hash sym)
  (let ((f (if (eq sym :167m)
	       #'figure-unit
	       #'figure5-unit)))
    (iter (for (code . shibu) :in-shibu :long)
	  (for c = (read-from-string code))
	  (appending (funcall f (funcall (gethash c hash) sym))
		     :into pot)
	  (finally (return (append pot
				   (funcall f (%total-type hash sym))))))))

(defun %list-abstract (list)
  (optima:match list
    ((LIST k _ d _ s _ t)
     (list k d s t))))

;; (list+ '((1 2 3) (4 5 6)) '((7 8 9) (10 11 12)))
;; -> '((8 10 12) (14 16 18))
(defun list+ (2dl1 2dl2)
  (iter (for i :from 0 :to (1- (length 2dl1)))
	(collect (mapcar #'+
			 (nth i 2dl1)
			 (nth i 2dl2)))))

(defun %spec-total (hash)
  (iter (with ret = nil)
	(for (code . shibu) :in-shibu :long)
	(for c   = (read-from-string code))
	(for %s% = (gethash c hash))
	(appending (funcall %s% :spec) :into pot)
	(setq ret (if ret
		      (list+ ret (funcall %s% :spec2))
		      (funcall %s% :spec2)))
	(finally (return (append pot
				 (mapcar #'%list-expand ret))))))

(defclass SHEET (R172T::SHEET)
  ((book         :initarg :book)
   (sheet        :initarg :sheet :accessor sheet-of)
   (name         :initarg :name)
   (borders-area :reader  borders->)
   (width        :initarg :width)
   (jpfont       :initarg :jpfont)
   (enfont)
   (c-align-area)
   (title        :initarg :title)
   (endcol       :initarg :endcol)
   (end          :initarg :end :reader end-of)
   (shibu-step   :initarg :shibu-step)
   (shibu-row    :initarg :shibu-row)
   (shibu-col	 :initarg :shibu-col)
   (mainhash	 :initarg :mainhash)
   (symbol	 :initarg :symbol)
   (data	 :initarg :data)
   (shibu-data)
   (merge-cells)))


(defun %title-end (title)
  (& excel::number-to-col length car title))

(defun make-borders-area (title)
  (format nil "A1:~A136" (%title-end title)))

(defun make-c-align (title)
  (list "A2:C136"
	(format nil "A1:~A1" (%title-end title))))

(defun make-enfont (title)
  (format nil "C2:~A136" (%title-end title)))

(defun make-merge-cells (symbol)
  (append (generate-merge-cells 5)
	  ;; (if (eq symbol :167m)
	  ;;     nil
	  ;;     (list "D1:E1" "F1:G1" "H1:I1" "J1:K1"))
	  (cond
	    ((eq symbol :167m) nil)
	    ((eq symbol :spec) (list "D1:E1" "F1:G1" "H1:I1"))
	    (t (list "D1:E1" "F1:G1" "H1:I1" "J1:K1")))))

(defun make-shibu-data ()
  (iter (for (code . shibu) :in-shibu :long)
	(appending
	 (list (list code shibu)
	       '("" "")
	       '("" "")
	       '("" "")
	       '("" "")))))

(defmethod initialize-instance :after ((s SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (sheet book data symbol mainhash title borders-area
		     name c-align-area enfont merge-cells shibu-data) s
    (setq sheet		(ole book :worksheets :item name)
	  ;; data		(%total mainhash symbol)
	  borders-area	(make-borders-area title)
	  c-align-area	(make-c-align title)
	  enfont	(make-enfont title)
	  merge-cells	(make-merge-cells symbol)
	  shibu-data	(make-shibu-data))))

(defpackage #:r172-spec
  (:nicknames :r172sp)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel :r172t)
  (:export #:filter
	   #:data))

(in-package :r172sp) ;----------------------------------------------------------------------

(defun filter (array)
  (mapcar (lambda (l)
	    (optima:match l
	      ((list* _ _ x1 x2 _) (list x1 x2))))
	  (r172t:init-list array 100)))

(defstruct (specsex
	     (:constructor specsex (dockm dockf scm scf totalm totalf)))
  shibuf shibum dockf dockm scf scm totalf totalm shibu dock sc total
  shibuf% shibum% dockf% dockm% scf% scm% totalf% totalm%)

(defun %create (line)
  (let1 obj (apply #'specsex line)
    (with-slots (shibuf totalf scf dockf shibum totalm scm dockm total dock sc shibu
			shibuf% shibum% dockf% dockm% scf% scm% totalf% totalm%)
	obj
      (setq shibuf  (- totalf scf dockf)
	    shibum  (- totalm scm dockm)
	    total   (+ totalf totalm)
	    shibu   (+ shibuf shibum)
	    dock    (+ dockf dockm)
	    sc      (+ scf scm)
	    shibuf% (percent-or-nil shibuf total)
	    shibum% (percent-or-nil shibum total)
	    dockf%  (percent-or-nil dockf total)
	    dockm%  (percent-or-nil dockm total)
	    scf%    (percent-or-nil scf total)
	    scm%    (percent-or-nil scm total)
	    totalf% (percent-or-nil totalf total)
	    totalm% (percent-or-nil totalm total)))
    obj))

(defun figure (spec)
  (declare (type specsex spec))
  (with-slots (shibum dockm scm totalm shibuf dockf scf totalf
	       shibum% dockm% scm% totalm% shibuf% dockf% scf% totalf%) spec
  `((,shibum ,shibum% ,dockm ,dockm% ,scm ,scm% ,totalm ,totalm%)
    (,shibuf ,shibuf% ,dockf ,dockf% ,scf ,scf% ,totalf ,totalf%))))

(defun data (dock sc main)
  (iter (for line :in (mapcar #'append dock sc main))
	(appending (figure (%create line)))))

(defun putdata-shibu ()
  (iter (for (code . shibu) :in-shibu :long)
	(appending (list (list code shibu)
			 (list "" "")))))

(defclass SHEET (R172T::SHEET)
  ((title
    :initform '(("支部CD" "支部" "支部健診" "" "ドック" "" "集合契約" "" "合計" "")))
   (borders-area :initform '("A1:J53"))
   (c-align-area :initform '("A2:B53" "A1:J1"))
   (merge-cells  :initform (append (generate-merge-cells 2)
				   (list "C1:D1" "E1:F1" "G1:H1" "I1:J1")))
   (enfont	 :initform '("C2:J53"))
   (data	 :initarg :data)
   (array	 :initarg :array)
   (dock	 :initarg :dock)
   (sc		 :initarg :sc)))

(defmethod initialize-instance :after ((m SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (dock sc array data title r172t::endcol r172t::end) m
    (setq data		(r172sp:data dock sc array)
	  r172t::endcol	(length (car title))
	  r172t::end	(excel::number-to-col r172t::endcol))))

(defpackage #:r172-internal
  (:nicknames :r172i)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel :r172t))

(in-package :r172i) ;----------------------------------------------------------------------

(defgeneric putData        (sh))
(defgeneric putBorder      (sh))
(defgeneric putCenterAlign (sh))
(defgeneric putEnFont      (sh))
(defgeneric execMerge      (sh))
(defgeneric PutShibu (obj))

(defmethod putData ((m R1721::SHEET))
  (with-slots (r172t::sheet r1721::data1 r1721::data2 r172t::title) m
    (decide-range-value r172t::sheet r172t::title :start-row 1)
    (decide-range-value r172t::sheet r1721::data1 :start-row 2)
    (decide-range-value r172t::sheet r172t::title :start-row 30)
    (decide-range-value r172t::sheet r1721::data2 :start-row 31)))
(defmethod putData ((m R172SP::SHEET))
  (with-slots (r172t::sheet r172sp::data r172sp::title) m
    (decide-range-value r172t::sheet r172sp::title :start-row 1)
    (decide-range-value r172t::sheet r172sp::data
			:start-row 2
			:start-column 3)
    (decide-range-value r172t::sheet (r172sp::putdata-shibu)
			:start-row 2)))
(defmethod putData ((m R1722::SHEET))
  (with-slots (r1722::sheet r1722::data r1722::title r1722::shibu-data) m
    (decide-range-value
     (r1722::sheet-of m)
     r1722::title
     :start-row 1
     :start-column 1)
    (decide-range-value
     (r1722::sheet-of m)
     r1722::data
     :start-row 2
     :start-column 4)
    (decide-range-value
     (r1722::sheet-of m)
     (repeated-list 27
		    (mapcar #'list '("合計" "本人" "家族" "男性" "女性")))
     :start-row 2
     :start-column 3)
    (decide-range-value
     (r1722::sheet-of m)
     r1722::shibu-data
     :start-row 2)))

(defmacro putAttribute (sym value attr &optional attr2)
  `(with-slots (,sym r172t::sheet) m
     (iter (for range :in ,sym)
	   ,(if attr2
		`(setf (slot-value (ole r172t::sheet :range range ,attr2) ,attr)
		       ,value)
		`(setf (slot-value (ole r172t::sheet :range range) ,attr) ,value)))))
(defmacro putAttribute2 (sym value attr &optional attr2)
  `(with-slots (,sym) m
     (iter (for range :in ,sym)
	   ,(if attr2
		`(setf (slot-value (ole (r1722::sheet-of m) :range range ,attr2) ,attr)
		       ,value)
		`(setf (slot-value (ole (r1722::sheet-of m) :range range) ,attr) ,value)))))

(defmethod putBorder ((m R172T::SHEET))
  (putAttribute r172t::borders-area 1 :LineStyle :borders))
(defmethod putBorder ((m R1721::SHEET))
  (putAttribute r1721::borders-area 1 :LineStyle :borders))
(defmethod putBorder ((m R172SP::SHEET))
  (putAttribute r172sp::borders-area 1 :LineStyle :borders))
(defmethod putBorder ((m R1722::SHEET))
  (with-slots (r1722::borders-area) m
    (setf (slot-value (slot-value (ole (r1722::sheet-of m) :range r1722::borders-area) :Borders)
		      :LineStyle)
	  1)))

(defmethod putCenterAlign ((m R172T::SHEET))
  (putAttribute r172t::c-align-area excel::xlcenter :HorizontalAlignment))
(defmethod putCenterAlign ((m R1721::SHEET))
  (putAttribute r1721::c-align-area excel::xlcenter :HorizontalAlignment))
(defmethod putCenterAlign ((m R172SP::SHEET))
  (putAttribute r172sp::c-align-area excel::xlcenter :HorizontalAlignment))
(defmethod putCenterAlign ((m R1722::SHEET))
  (putAttribute2 r1722::c-align-area excel::xlcenter :HorizontalAlignment))

(defmethod putEnFont ((m R172T::SHEET))
  (putAttribute r172t::enfont "Times New Roman" :name :font))
(defmethod putEnFont ((m R1721::SHEET))
  (putAttribute r1721::enfont "Times New Roman" :name :font))
(defmethod putEnFont ((m R172SP::SHEET))
  (putAttribute r172sp::enfont "Times New Roman" :name :font))
(defmethod putEnFont ((m R1722::SHEET))
  (with-slots (r1722::enfont) m
    (setf (slot-value
	   (slot-value (ole (r1722::sheet-of m) :range r1722::enfont)
		       :font)
	   :name)
	  "Times New Roman")))

(defmethod execMerge ((m R172T::SHEET))
  (putAttribute r172t::merge-cells t :MergeCells))
(defmethod execMerge ((m R1721::SHEET))
  (putAttribute r1721::merge-cells t :MergeCells))
(defmethod execMerge ((m R172SP::SHEET))
  (putAttribute r172sp::merge-cells t :MergeCells))
(defmethod execMerge ((m R1722::SHEET))
  (putAttribute2 r1722::merge-cells t :MergeCells))

(defun %type1-sheet (book ary name category &key (step 5))
  (let1 obj (make-instance 'R1721::SHEET
			   :book      book
			   :name      name
			   :category  category
			   :step      step
			   :mainarray ary)
    (putData        obj)
    (putBorder      obj)
    (putCenterAlign obj)
    (putEnFont      obj)))

(defun %sex-year-sheet (book ary &key (step 5))
  (%type1-sheet book ary "性別集計表" '("男性" "女性") :step step))

(defun %hk-sheet (book ary &key (step 5))
  (%type1-sheet book ary "本人・家族別集計表" '("本人" "家族") :step step))

(defun %spec-sheet (book name dock sc main)
  (let1 obj (make-instance 'R172SP::SHEET
			   :book  book
			   :name  name
			   :array main
			   :dock  dock
			   :sc    sc)
    (putData        obj)
    (putBorder      obj)
    (putCenterAlign obj)
    (putEnFont      obj)
    (execMerge	    obj)))

;; (defparameter x (r1721::dock-read
;; 		 (cl-store:restore ksetting::*zenken-hash*)))

(defun spec (book sexmain hkmain)
  (let* ((hash     (cl-store:restore ksetting::*zenken-hash*))
	 (dock     (r1721::dock-read hash))
	 (dock-sex (r172sp::filter (r1721::sex-array dock)))
	 (dock-hk  (r172sp::filter (r1721::hk-array dock)))
	 (sc       (r1721::sc-read hash))
	 (sc-sex   (r172sp::filter (r1721::sex-array sc)))
	 (sc-hk    (r172sp::filter (r1721::hk-array sc)))
	 (smain    (r172sp::filter sexmain))
	 (hkmain   (r172sp::filter hkmain)))
    (%spec-sheet book "内訳表(性別)" dock-sex sc-sex smain)
    (%spec-sheet book "内訳表(本人・家族別)" dock-hk sc-hk hkmain)))

(defun type2-spec (book hash)
  (make-instance 'r1722::sheet
		 :book book
		 :name  "内訳表"
		 :title '(("支部CD" "支部" "" "支部健診" ""
			   "人間ドック" "" "特定健診" ""
			    "合計"))
		 :data  (r1722::%spec-total hash)
		 :symbol :spec))

(defun type2-hsido (book hash)
  (make-instance 'r1722::sheet
		 :book book
		 :name  "保健指導レベル別"
		 :title '(("支部CD" "支部" "" "積極的支援" ""
			   "動機付支援" "" "なし" ""
			   "その他" "" "合計"))
		 ;; :mainhash hash
		 :symbol :hsido
		 :data  (r1722::%total hash :hsido)))

(defun type2-167month (book hash)
  (make-instance 'r1722::sheet
		 :book book
		 :name  "月別"
		 :title '(("支部CD" "支部" ""
			   "4月" "5月" "6月" "7月" "8月" "9月"
			   "10月" "11月" "12月" "1月" "2月" "3月"
			   "合計"))
		 :data  (r1722::%total hash :167m)
		 :symbol :167m))

(defun type2-metabo (book hash)
  (make-instance 'r1722::sheet
		 :book book
		 :name  "メタボレベル別"
		 :title '(("支部CD" "支部" ""
			   "基準該当" "" "予備群該当" ""
			   "情報提供" "" "その他" ""
			   "合計"))
		 :data  (r1722::%total hash :metabo)
		 :symbol :metabo))

(defun type2 (book 172data 167hash)
  (let* ((hash  (R1722::%CLASSIFY 172data 167hash)))
    (dolist (obj (list (type2-spec book hash)
		       (type2-167month book hash)
		       (type2-hsido book hash)
		       (type2-metabo book hash)))
      (putData        obj)
      (putBorder      obj)
      (putCenterAlign obj)
      (putEnFont      obj)
      (execMerge    obj))))

(in-package :kensin) ;----------------------------------------------------------------------

(defparameter width1 #(8 10 7 8 8 3 9 8 13 8 8 12 3 3 8 20 12 9 3 3 8 20))

(defparameter %visible%		t)
(defparameter %quit%		nil)
(defparameter %debugger%	t)
(defparameter %close%		nil)

(defmacro with-172-xls ((book-sym 172file 167file) &rest body)
  `(excel:with-excel
       (app :visible %visible% :quit nil :debugger %debugger%)
     (excel:with-excel-book
	 (app ,book-sym _172file_ :close nil :debugger %debugger%)
       ,@body)))

(defun %file->csv (file)
  (nthcdr 2
	  (csv-read-to-list (or file
				(172-newest))
			    :code :SJIS)))

(defun %csvfile (csv)
  (iter (for line :in csv)
	(for obj = (create-172data line))
	(if (172-target? obj)
	    (collect obj))))

(defun %167 ()
  (iter (with hash = (cl-store:restore #P"f:/167.2012.hash"))
	(for line :in (%csvfile #P"f:/FKCA172_2012.csv"))
	(for obj = (create-172data line))
	(for jnum = (172data-受診券整理番号 obj))
	(if (172-target? obj)
	    (collect (gethash jnum hash)))))

(defun 167-target (172-csvdata 167hash)
  (iter (with hash = 167hash)
	(for line :in 172-csvdata)
	(for obj = (create-172data line))
	(for jnum = (172data-受診券整理番号 obj))
	(if (172-target? obj)
	    (collect (gethash jnum hash)))))

(defmacro 172-thread (form)
  `(sb-thread:join-thread
    (sb-thread:make-thread (lambda () ,form))))

(defmacro forked-thread (form)
  `(sb-thread:make-thread
    (lambda () ,form)))

(defun get-thread-value (thread)
  (sb-thread::join-thread thread))

(defun 172-xls-main (&key 172file 167file)
  ;; (declare (optimize safety debug))
  (declare (optimize speed))
  (let* ((_172file_ (or 172file ksetting::*fkca172*))
	 (_167file_ (or 167file ksetting::*fkac167*))
	 (csvdata (forked-thread (%file->csv _172file_)))
	 (csv     (forked-thread (%csvfile (get-thread-value csvdata))))
	 (167hash (forked-thread (r167-hash _167file_)))
	 (class   (forked-thread (r172t::classify csvdata))))
    (with-172-xls (book 172file 167file)
      (let* ((sh      (ole book :worksheets :item 1))
    	     (lr      (lastrow sh :y 1 :x 1)))
    	(set-column-width sh (:a :v) width1)
    	(border sh (:a 2) (:v (1- (the fixnum lr))))
    	(prog1
    	    (multiple-value-bind (s k1 k2 syhash hkarray hlvhash hlvary)
    		(sb-thread::join-thread class)
    	      (declare (ignorable s k1 k2 syhash hkarray hlvhash hlvary))
    	      ;; (r172i::%sex-year-sheet book syhash :step 5)
    	      ;; (r172i::%hk-sheet       book syhash :step 5)
    	      (r172i::type2 book
			    (get-thread-value csv)
			    (get-thread-value 167hash)))
    	  (excel::save-book book _172file_ :xlsx))))))

;; (defun %%172-xls-main (&key 172file 167file)
;;   ;; (declare (optimize safety debug))
;;   (declare (optimize speed))
;;   (let* ((_172file_ (or 172file ksetting::*fkca172*))
;; 	 (_167file_ (or 167file ksetting::*fkac167*))
;; 	 (csvdata (%file->csv _172file_))
;; 	 (csv     (%csvfile csvdata))
;; 	 (167hash (r167-hash _167file_)))
;;     (with-172-xls (book 172file 167file)
;;       (let* ((sh      (ole book :worksheets :item 1))
;;     	     (lr      (lastrow sh :y 1 :x 1)))
;;     	(set-column-width sh (:a :v) width1)
;;     	(border sh (:a 2) (:v (1- lr)))
;;     	(prog1
;;     	    (multiple-value-bind (s k1 k2 syhash hkarray hlvhash hlvary)
;;     		(r172t::classify csvdata)
;;     	      (declare (ignorable s k1 k2 syhash hkarray hlvhash hlvary))
;;     	      ;; (r172i::%sex-year-sheet book syhash :step 5)
;;     	      ;; (r172i::%hk-sheet       book syhash :step 5)
;;     	      (r172i::type2 book csv 167hash))
;;     	  (excel::save-book book _172file_ :xlsx))))))

(defparameter 172csv (%csvfile (%file->csv ksetting::*fkca172*)))
(defparameter 167hash (r167-hash ksetting::*fkac167*))

(in-package :cl-user)
