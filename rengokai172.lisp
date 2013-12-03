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

;; (defun 172-hsido-total-output-xml (plist)
;;   (let* ((l1	(getf plist :level1))
;; 	 (l1u	(getf plist :level1-user))
;; 	 (l1f	(getf plist :level1-finisher))
;; 	 (l2	(getf plist :level2))
;; 	 (l2u	(getf plist :level2-user))
;; 	 (l2f	(getf plist :level2-finisher)))
;;     (with-output-to-string (i "")
;;       (cl-who:with-html-output (i)
;; 	(cl-who:htm (:root
;; 		     (:level1 :total l1 :user l1u :finisher l1f)
;; 		     (:level2 :total l2 :user l2u :finisher l2f)))))))

(defun 172-hsido-total (&optional (output-type :identity))
  "output-type  :identity :csv :xml :yaml"
  (let* ((mainhash (172-hsido-classify))
	 (level1   (gethash "1" mainhash))
	 (level2   (gethash "2" mainhash)))
    (funcall
     (case output-type
       (:identity	#'identity)
       (:csv		#'172-hsido-total-output-csv)
       ;; (:xml		#'172-hsido-total-output-xml)
       )
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

(defpackage #:r172-hsido
  (:nicknames :r172h)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel :r172t))

(in-package :r172h) ;----------------------------------------------------------------------

(defstruct hsido code name h k m f total size)

(declaim (inline add-hsido))

(defun create-hsido (code name size)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum code size)
	   (type simple-string name))
  (let1 obj (make-hsido :code code :name name :size size)
    (with-slots (h k m f total size) obj
      (setq h     (make-array (list size) :initial-element 0)
	    k     (make-array (list size) :initial-element 0)
	    m     (make-array (list size) :initial-element 0)
	    f     (make-array (list size) :initial-element 0)
	    total (make-array (list size) :initial-element 0))
      (the hsido obj))))

(defun add-hsido (obj hlv hk sex)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type hsido obj)
	   (type fixnum hlv hk sex))
  (with-slots (h k m f total size) obj
    (let ((vhk  (svref (vector 0 h k) hk))
	  (vsex (svref (vector 0 m f) sex))
	  (lv   (1- hlv)))
      (aref-1+ vhk lv)
      (aref-1+ vsex lv)
      (aref-1+ total lv)
      (aref-1+ vhk (1- size))
      (aref-1+ vsex (1- size))
      (aref-1+ total (1- size)))))

(defun generate-shibu-hash (size)
  (iter (with hash = (make-hash-table :test #'equal))
	(for (code . shibu) :in-shibu :long)
	(for c = (read-from-string code))
	(setf (gethash c hash)
	      (create-hsido c shibu size))
	(finally (return hash))))

(defun %calculate (csvdata)
  (iter (with hash = (generate-shibu-hash 5))
	(for line :in csvdata)
	(with-slots (kensin::支部
		     kensin::性別
		     kensin::受診券整理番号
		     kensin::保健指導レベル) line
	  (for shibu = (read-from-string kensin::支部))
	  (for sex   = (read-from-string kensin::性別))
	  (for hk    = (hk kensin::受診券整理番号))
	  (for hlv   = (read-from-string kensin::保健指導レベル))
	  (add-hsido (gethash shibu hash) hlv hk sex))
	(finally (return hash))))

(defun vector-figure (vec)
  (optima:match vec
    ((vector lv1 lv2 none other total)
     (list lv1 (percent-or-nil lv1 total)
	   lv2 (percent-or-nil lv2 total)
	   none (percent-or-nil none total) 
	   other (percent-or-nil other total) 
	   total))))

(defun figure (hsido)
  (with-slots (code name h k m f total) hsido
    (iter (for v :in (list total h k m f))
	  (if (first-time-p)
	      (collect `(,code ,name "" ,@(vector-figure v)))
	      (collect `("" "" "" ,@(vector-figure v)))))))

(defun shibu-data-total (hash)
  (let (th tk tm tf tt)
    (iter (for (k v) :in-hashtable hash)
	  (with-slots (h k m f total) v
	    (setf th (if th (vector-sum th h) h)
		  tk (if tk (vector-sum tk k) k)
		  tm (if tm (vector-sum tm m) m)
		  tf (if tf (vector-sum tf f) f)
		  tt (if tt (vector-sum tt total) total))))
    (mapcar (lambda (v) `("" "" "" ,@(vector-figure v)))
	    (list tt th tk tm tf))))

(defun data (hash)
  (iter (for (code . shibu) :in-shibu :long)
	(for c = (read-from-string code))
	(appending (figure (gethash c hash))
		   :into pot)
	(finally (return (append pot
				 (shibu-data-total hash))))))


(defclass SHEET (R172T::SHEET)
  ((title
    :initform '(("支部CD" "支部" "" "積極的支援" "" "動機付支援" "" "情報提供" "" "その他" "" "合計")))
   (borders-area :initform '("A1:L136"))
   (c-align-area :initform '("A2:C136" "A1:L1"))
   (enfont	 :initform '("C2:L136"))
   (data	 :initarg :data)
   (merge-cells  :initform (append (generate-merge-cells 5)
				   (list "D1:E1" "F1:G1" "H1:I1" "J1:K1")))
   (172data	 :initarg :172data)))

(defmethod initialize-instance :after ((m SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (sexmain hkmain hlv title data 172data r172t::endcol r172t::end) m
    (setq data		(data (%calculate 172data))
	  r172t::endcol	(length (car title))
	  r172t::end	(excel::number-to-col r172t::endcol))))

(defpackage #:r172-167
  (:nicknames :r167)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel :r172t))

(in-package :r167) ;----------------------------------------------------------------------

(declaim (inline create-167shibu add-167shibu))

(defun %month (r167)
  (with-slots (kensin::実施月) r167
    (if (> 4 kensin::実施月)
	(+ kensin::実施月 8)
	(- kensin::実施月 4))))

(defun %r172-base-info (r172)
  (with-slots (kensin::受診券整理番号
	       kensin::性別
	       kensin::被保険者証記号
	       kensin::被保険者証番号) r172
    (values kensin::受診券整理番号
	    (read-from-string
	     (kensin::shibu kensin::被保険者証記号 kensin::被保険者証番号))
	    (hk kensin::受診券整理番号)
	    (read-from-string kensin::性別))))

(defstruct 167shibu code name h k m f total)

(defun create-167shibu (code name size)
  (let1 obj (make-167shibu :code code :name name)
    (with-slots (h k m f total) obj
      (setq h		(make-array `(,size) :initial-element 0)
	    k		(make-array `(,size) :initial-element 0)
	    m		(make-array `(,size) :initial-element 0)
	    f		(make-array `(,size) :initial-element 0)
	    total	(make-array `(,size) :initial-element 0))
      obj)))

(defun add-167shibu (obj hk sex)
  ;; (declare (optimize speed) (type 167shibu obj)
  ;; 	   (type fixnum hk sex))
  (lambda (month)
    (with-slots (h k m f total) obj
      (let ((hkary  (vector 0 h k))
	    (sexary (vector 0 m f)))
	(aref-1+ (svref hkary hk) month)
	(aref-1+ (svref sexary sex) month)
	(aref-1+ total month)
	(aref-1+ (svref hkary hk) 12)
	(aref-1+ (svref sexary sex) 12)
	(aref-1+ total 12)))))

(defun generate-shibu-hash (size)
  (iter (with hash = (make-hash-table :test #'equal))
	(for (code . shibu) :in-shibu :long)
	(for c = (read-from-string code))
	(setf (gethash c hash)
	      (create-167shibu c shibu size))
	(finally (return hash))))

(defun %classify (172data 167hash size function 167function)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (iter (with hash = (generate-shibu-hash size))
	(for obj :in 172data)
	(multiple-value-bind (jnum shibu-code hk sex)
		(%r172-base-info obj)
	      (for r167  = (gethash jnum 167hash))
	      (funcall (funcall function (gethash shibu-code hash) hk sex)
		       (funcall 167function r167)))
	(finally (return hash))))

(defun figure (r167)
  (declare (optimize speed) (type 167shibu r167))
  (with-slots (h k m f total) r167
    (mapcar (lambda (v) (coerce v 'list))
	    (list total h k m f))))

(defun shibu-data-total (hash)
  (let (th tk tm tf tt)
    (iter (for (k v) :in-hashtable hash)
	  (with-slots (h k m f total) v
	    (setf th (if th (vector-sum th h) h)
		  tk (if tk (vector-sum tk k) k)
		  tm (if tm (vector-sum tm m) m)
		  tf (if tf (vector-sum tf f) f)
		  tt (if tt (vector-sum tt total) total))))
    (mapcar (lambda (v) (coerce v 'list))
	    (list tt th tk tm tf))))

(defun shibu-data (hash)
  (iter (for (code . shibu) :in-shibu :long)
	(for c = (read-from-string code))
	(appending (figure (gethash c hash)) :into pot)
	(finally (return (append pot
				 (shibu-data-total hash))))))

(defclass SHEET (R172T::SHEET)
  ((title	 :initform (list (nendo-month-list :format "~A月")))
   (borders-area :initform '("A1:P136"))
   (enfont	 :initform '("C2:P136"))
   (c-align-area :initform '("A2:C136" "A1:P1"))
   (data	 :initarg :data)
   (merge-cells  :initform (generate-merge-cells 5))
   (step1	 :initarg :step1)
   (step2	 :initarg :step2)
   (172data	 :initarg :172data)
   (167hash	 :initarg :167hash)
   (shibu-step   :initform 5)
   (shibu-row	 :initform 2)
   (shibu-col	 :initform :a)))

(defmethod initialize-instance :after ((m SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (172data 167hash data) m
    (setq data   (shibu-data
		  (%classify 172data 167hash 13
			     #'add-167shibu #'%month)))))

(defpackage #:r172-metabo
  (:nicknames :r172m)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel :r172t))

(in-package :r172m) ;----------------------------------------------------------------------

(defun %metabo-lv (r167)
  (car (kensin::r167-メタボレベル r167)))

(defun add-172metabo (obj hk sex)
  (lambda (mlv)
    (with-slots (r167::h r167::k r167::m r167::f r167::total) obj
      (let ((hkary  (vector 0 r167::h r167::k))
	    (sexary (vector 0 r167::m r167::f)))
	(aref-1+ (svref hkary hk) (1- mlv))
	(aref-1+ (svref sexary sex) (1- mlv))
	(aref-1+ r167::total (1- mlv))
	(aref-1+ (svref hkary hk) 3)
	(aref-1+ (svref sexary sex) 3)
	(aref-1+ r167::total 3)))))

(defun figure-vector (vec)
  (iter (with last = (svref vec 3))
	(for n :from 0 :to 2)
	(for s = (svref vec n))
	(appending (list s (percent-or-nil s last))
		   :into pot)
	(finally (return (append pot (list last))))))

(defun figure (r167)
  (with-slots (r167::code r167::name
	       r167::h r167::k r167::m r167::f r167::total) r167
    `((,r167::code ,r167::name "" ,@(figure-vector r167::total))
      ("" "" "" ,@(figure-vector r167::h))
      ("" "" "" ,@(figure-vector r167::k))
      ("" "" "" ,@(figure-vector r167::m))
      ("" "" "" ,@(figure-vector r167::f)))))

(defun shibu-data (hash)
  (iter (for (code . shibu) :in-shibu :long)
	(for c = (read-from-string code))
	(appending (figure (gethash c hash)))))


(defun %classify (172data 167hash)
  (iter (with sexary = (make-array '(96 3 5) :initial-element 0))
	(with hkary  = (make-array '(96 3 5) :initial-element 0))
	(for obj :in 172data)
	(with-slots (kensin::受診券整理番号
			 kensin::性別
			 kensin::被保険者証記号
			 kensin::被保険者証番号) obj
	      (for shibu = (read-from-string
			    (kensin::shibu kensin::被保険者証記号 kensin::被保険者証番号)))
	      (for hk    = (hk kensin::受診券整理番号))
	      (for sex   = (read-from-string kensin::性別))
	      (for r167  = (gethash kensin::受診券整理番号 167hash))
	      (for lv    = (car (kensin::r167-メタボレベル r167)))
	      (aref-1+ sexary shibu sex lv)
	      (aref-1+ hkary  shibu hk lv))
	(finally (return (values hkary sexary)))))

(defstruct shibu
  code name
  lv1h lv1k lv1f lv1m lv1t
  lv2h lv2k lv2f lv2m lv2t
  oth otk otf otm ott
  th tk tf tm ttt)

(defun create-shibu (code name hkary sexary)
  (let1 obj (make-shibu :code code :name name)
    (multiple-value-bind (h1 h2 ho k1 k2 ko) (%get-value code hkary)
      (multiple-value-bind (m1 m2 mo f1 f2 fo) (%get-value code sexary)
	(with-slots (lv1h lv1k lv1f lv1m lv1t
			  lv2h lv2k lv2f lv2m lv2t
			  oth otk otf otm ott
			  th tk tf tm ttt) obj
	  (setq lv1h h1
		lv1k k1
		lv1f f1
		lv1m m1
		lv1t (+ h1 k1)
		lv2h h2
		lv2k k2
		lv2f f2
		lv2m m2
		lv2t (+ h2 k2)
		oth  ho
		otk  ko
		otf  fo
		otm  mo
		ott  (+ ho ko)
		th   (+ h1 h2 ho)
		tk   (+ k1 k2 ko)
		tf   (+ f1 f2 fo)
		tm   (+ m1 m2 mo)
		ttt  (+ lv1t lv2t ott)))))
    obj))

(defun %get-value (shibu-code array)
  (let ((start (+ (* shibu-code 15) 6)))
    (values (row-major-aref array start)
	    (row-major-aref array (+ start 1))
	    (row-major-aref array (+ start 2))
	    (row-major-aref array (+ start 5))
	    (row-major-aref array (+ start 6))
	    (row-major-aref array (+ start 7)))))

(defmacro figure-expand (sym)
  (let ((lv1 (intern (format nil "LV1~A" sym)))
	(lv2 (intern (format nil "LV2~A" sym)))
	(ot  (intern (format nil "OT~A" sym)))
	(tt  (intern (format nil "T~A" sym))))
  `(list "" "" ""
	 ,lv1 (percent-or-nil ,lv1 ,tt)
	 ,lv2 (percent-or-nil ,lv2 ,tt)
	 ,ot (percent-or-nil ,ot ,tt)
	 ,tt)))

(defun figure2 (shibu)
  (declare (type shibu shibu))
  (with-slots (code name lv1h lv1k lv1f lv1m lv1t
		    lv2h lv2k lv2f lv2m lv2t
		    oth otk otf otm ott th tk tf tm ttt) shibu
    (list (list code name ""
		lv1t (percent-or-nil lv1t ttt) lv2t (percent-or-nil lv2t ttt)
		ott (percent-or-nil ott ttt) ttt)
	  (figure-expand :H)
	  (figure-expand :K)
	  (figure-expand :M)
	  (figure-expand :F))))

(defun %shibufy (172data 167hash)
  (multiple-value-bind (hk sex) (%classify 172data 167hash)
    (iter (for (code . sh) :in-shibu :long)
	  (for c = (read-from-string code))
	  (appending (figure2 (create-shibu c sh hk sex))))))

(defclass SHEET (R172T::SHEET)
  ((title
    :initform '(("支部CD" "支部" "" "基準該当" "" "予備群該当" "" "その他" "" "合計")))
   (borders-area :initform '("A1:J131"))
   (c-align-area :initform '("A2:C131" "A1:J1"))
   (enfont	 :initform '("C2:J131"))
   (172data	 :initarg :172data)
   (167hash	 :initarg :167hash)
   (data	 :initarg :data)
   (merge-cells  :initform (append (generate-merge-cells 5)
				   (list "D1:E1" "F1:G1" "H1:I1")))))

(defmethod initialize-instance :after ((m SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (data title r172t::endcol r172t::end
		    172data 167hash) m
    (setq r172t::endcol	(length (car title))
	  r172t::end	(excel::number-to-col r172t::endcol)
	  data		(shibu-data
			 (r167::%classify 172data 167hash
					  4
					  #'add-172metabo
					  #'%metabo-lv)))))

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
(defmethod putData ((m R172H::SHEET))
  (with-slots (r172t::sheet r172h::data r172h::title) m
    (decide-range-value r172t::sheet r172h::title :start-row 1)
    (decide-range-value r172t::sheet r172h::data
			:start-row 2)
    (decide-range-value
     r172t::sheet
     (repeated-list 27 (mapcar #'list '("合計" "本人" "家族" "男性" "女性")))
     :start-row 2
     :start-column 3)))
(defmethod putData ((m R167::SHEET))
  (with-slots (r172t::sheet r167::data r167::title) m
    (decide-range-value r172t::sheet r167::title
			:start-row 1
			:start-column 4)
    (decide-range-value r172t::sheet r167::data
    			:start-row 2
    			:start-column 4)
    (decide-range-value
     r172t::sheet
     (repeated-list 27 (mapcar #'list '("合計" "本人" "家族" "男性" "女性")))
     :start-row 2
     :start-column 3)))
(defmethod putData ((m R172m::SHEET))
  (with-slots (r172t::sheet r172m::data r172m::title) m
    (decide-range-value r172t::sheet r172m::title :start-row 1)
    (decide-range-value r172t::sheet r172m::data
			:start-row 2)
    (decide-range-value
     r172t::sheet
     (repeated-list 27 (mapcar #'list '("合計" "本人" "家族" "男性" "女性")))
     :start-row 2
     :start-column 3)))

(defmacro putAttribute (sym value attr &optional attr2)
  `(with-slots (,sym r172t::sheet) m
     (iter (for range :in ,sym)
	   ,(if attr2
		`(setf (slot-value (ole r172t::sheet :range range ,attr2) ,attr)
		       ,value)
		`(setf (slot-value (ole r172t::sheet :range range) ,attr) ,value)))))

(defmethod putBorder ((m R172T::SHEET))
  (putAttribute r172t::borders-area 1 :LineStyle :borders))
(defmethod putBorder ((m R172SP::SHEET))
  (putAttribute r172sp::borders-area 1 :LineStyle :borders))
(defmethod putBorder ((m R172H::SHEET))
  (putAttribute r172h::borders-area 1 :LineStyle :borders))
(defmethod putBorder ((m R167::SHEET))
  (putAttribute r167::borders-area 1 :LineStyle :borders))
(defmethod putBorder ((m R172M::SHEET))
  (putAttribute r172m::borders-area 1 :LineStyle :borders))

(defmethod putCenterAlign ((m R172T::SHEET))
  (putAttribute r172t::c-align-area excel::xlcenter :HorizontalAlignment))
(defmethod putCenterAlign ((m R172SP::SHEET))
  (putAttribute r172sp::c-align-area excel::xlcenter :HorizontalAlignment))
(defmethod putCenterAlign ((m R172H::SHEET))
  (putAttribute r172h::c-align-area excel::xlcenter :HorizontalAlignment))
(defmethod putCenterAlign ((m R167::SHEET))
  (putAttribute r167::c-align-area excel::xlcenter :HorizontalAlignment))
(defmethod putCenterAlign ((m R172M::SHEET))
  (putAttribute r172m::c-align-area excel::xlcenter :HorizontalAlignment))

(defmethod putEnFont ((m R172T::SHEET))
  (putAttribute r172t::enfont "Times New Roman" :name :font))
(defmethod putEnFont ((m R172SP::SHEET))
  (putAttribute r172sp::enfont "Times New Roman" :name :font))
(defmethod putEnFont ((m R172H::SHEET))
  (putAttribute r172h::enfont "Times New Roman" :name :font))
(defmethod putEnFont ((m R167::SHEET))
  (putAttribute r167::enfont "Times New Roman" :name :font))
(defmethod putEnFont ((m R172M::SHEET))
  (putAttribute r172m::enfont "Times New Roman" :name :font))

(defmethod execMerge ((m R172T::SHEET))
  (putAttribute r172t::merge-cells t :MergeCells))
(defmethod execMerge ((m R172SP::SHEET))
  (putAttribute r172sp::merge-cells t :MergeCells))
(defmethod execMerge ((m R172H::SHEET))
  (putAttribute r172h::merge-cells t :MergeCells))
(defmethod execMerge ((m R167::SHEET))
  (putAttribute r167::merge-cells t :MergeCells))
(defmethod execMerge ((m R172M::SHEET))
  (putAttribute r172m::merge-cells t :MergeCells))

(defmethod  PutShibu ((obj R172T::SHEET))
  (with-slots (r167::shibu-step r167::shibu-row r167::shibu-col r172t::sheet) obj
      (iter (with start-from = r167::shibu-row)
	    (for (code . shibu) :in-shibu :long)
	    (excel::value! r172t::sheet
			   (r167::shibu-col start-from)
			   ((excel::number-to-col
			     (1+ (excel::col-to-number r167::shibu-col)))
			    start-from)
			   (list code shibu))
	    (setf start-from (+ r167::shibu-step start-from)))))

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

(defun %hsido-sheet (book 172data)
  (let1 obj (make-instance 'R172H::SHEET
			   :book    book
			   :name    "保健指導レベル別"
			   :172data 172data)
    (putData        obj)
    (putBorder      obj)
    (putCenterAlign obj)
    (putEnFont      obj)
    (execMerge	    obj)))

(defun %167sheet (book 172data 167hash)
  (declare (optimize safety debug))
  (let1 obj (make-instance 'R167::SHEET
			   :book book
			   :name "月別集計表"
			   :172data 172data
			   :167hash 167hash)
    (putData        obj)
    (putBorder      obj)
    (putCenterAlign obj)
    (putEnFont      obj)
    (putShibu	    obj)
    (execMerge	    obj)))

(defun %metabo-sheet (book 172data 167hash)
  (let1 obj (make-instance 'R172M::SHEET
			   :book book
			   :name "メタボリック判定別"
			   :172data 172data
			   :167hash 167hash)
    (putData        obj)
    (putBorder      obj)
    (putCenterAlign obj)
    (putEnFont      obj)
    (execMerge	    obj)))

(in-package :kensin) ;----------------------------------------------------------------------

(defparameter width1 #(8 10 7 8 8 3 9 8 13 8 8 12 3 3 8 20 12 9 3 3 8 20))

(defparameter %visible%		t)
(defparameter %quit%		nil)
(defparameter %debugger%	t)
(defparameter %close%		nil)

(defmacro with-172-xls ((book-sym 172file 167file) &rest body)
  `(excel:with-excel
       (app :visible %visible% :quit nil :debugger %debugger%)
     (let ((_172file_ (or ,172file ,ksetting::*fkca172*))
	   (_167file_ (or ,167file ,ksetting::*fkac167*)))
       (excel:with-excel-book
	   (app ,book-sym _172file_ :close nil :debugger %debugger%)
	 ,@body))))

;; ()
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

(defun 172-xls-main (&key 172file 167file)
  (declare (optimize safety debug))
  (with-172-xls (book 172file 167file)
    (let* ((sh      (ole book :worksheets :item 1))
	   (lr      (lastrow sh :y 1 :x 1))
	   (csvdata (%file->csv _172file_))
	   (csv     (%csvfile csvdata))
	   (167hash (r167-hash _167file_)))
      (set-column-width sh (:a :v) width1)
      (border sh (:a 2) (:v (1- lr)))
      (prog1
      	  (multiple-value-bind (s k1 k2 syhash hkarray hlvhash hlvary)
      	      (r172t::classify csvdata)
      	    (declare (ignorable s k1 k2 syhash hkarray hlvhash hlvary))
      	    (r172i::%sex-year-sheet book syhash :step 5)
      	    (r172i::%hk-sheet       book syhash :step 5)
      	    (r172i::spec	      book syhash hkarray)
	    (r172i::%hsido-sheet    book csv)
	    (r172i::%167sheet book csv 167hash)
      	    (r172i::%metabo-sheet book csv 167hash))
	(excel::save-book book _172file_ :xlsx)))))

(defun 172base ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in-csv #P"f:/FKCA172_2012.csv" :code :SJIS)
	(optima:match line
	  ((LIST _)		  (next-iteration))
	  ((LIST* _ "FKCA172" _)  (next-iteration))
	  ((LIST* "保険者番号" _) (next-iteration))
	  (_
	   (let ((obj (create-172data line)))
	     (if (172-target? obj)
		 (setf (gethash (172data-指導メッセージID obj) hash)
		       (cons obj (gethash (172data-指導メッセージID obj) hash)))))))
	(finally (return hash))))

;; (defparameter 172csv  (%csvfile (%file->csv ksetting::*fkca172*)))
;; (defparameter 167hash (r167-hash ksetting::*fkac167*))

(in-package :cl-user)
