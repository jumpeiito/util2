(in-package #:check-code) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline string-drop-right))

(defparameter xls-checker-verbose nil)

(defun xls-checker-format (string)
  (if xls-checker-verbose
      (format t string)))

(defun string-drop-right (string n)
  (declare (optimize speed)
	   (type simple-string string)
	   (type fixnum n))
  (subseq string 0 (- (length string) n)))

(xrep::defclass% Code ()
  (code		code->)
  (must		must->)
  (name1	name1->)
  (name2	name2->)
  (strtype	strtype->)
  (formatta	formatta->)
  (xml-type	xml-type->)
  (unit		unit->)
  (explain	explain->)
  (min		min->)
  (max		max->))

(xrep::defclass% CodeString (Code))
(xrep::defclass% CodeNumber (Code))
(xrep::defclass% CodeCode   (Code)
  (selectable	select->))

(defgeneric create (obj line))
(defmethod create ((obj Code) (line LIST))
  (setf (code-> obj)		(nth 0 line)
	(must-> obj)		(nth 1 line)
	(name1-> obj)		(nth 2 line)
	(name2-> obj)		(nth 3 line)
	(strtype-> obj)		(nth 4 line)
	(formatta-> obj)	(nth 6 line)
	(xml-type-> obj)	(nth 7 line)
	(unit-> obj)		(nth 8 line)
	(explain-> obj)		(nth 19 line)
	(min-> obj)		(nth 23 line)
	(max-> obj)		(nth 24 line))
  obj)

(defmethod create :after ((cd CodeCode) (line LIST))
  "クラス変数explainには「1:あり 2:なし」のような文字列が入っているので、
そこから(1 2)を抜き出す。"
  (setf (select-> cd)
	(mapcar (lambda (c) (string-drop-right c 1))
		(ruby-scan "[0-9]+:" (explain-> cd))))
  cd)

(defmethod create ((n NULL) (line LIST))
  nil)

(defun make-obj (type)
  (string-case type
    ("数字"   (make-instance 'CodeNumber))
    ("文字列" (make-instance 'CodeString))
    ("コード" (make-instance 'CodeCode))
    (t        nil)))

;; 
(defun info (book)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (let* ((sh (ole book :worksheets :item "健診項目コード表"))
	 (lr (lastrow sh :y 3 :x 30)))
    (iter (with hash = (make-hash-table :test #'equal))
    	  (for line :in (value sh (:ad 3) (:bb lr)))
    	  (for obj = (make-obj (nth 4 line)))
   	  (create obj line)
    	  (if obj
    	      (sethash (code-> obj) obj hash))
    	  (finally (return hash)))))

(in-package #:check) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter hospital-alist
  '(("2610903946" . "城　南")
    ("2610803013" . "東　山")
    ("2614102230" . "大　宅")
    ("2610201358" . "上　京")
    ("2613000625" . "新河端")
    ("2610403590" . "西七条")
    ("2610500916" . "吉祥院")
    ("2620700092" . "事業団")
    ("2610601342" . "第二中")
    ("2610406569" . "西七条")
    ("2610606093" . "洛　北")
    ("2613300108" . "たんご")
    ("2610307411" . "太子道")
    ("2611801123" . "京協立")
    ("2612701488" . "まい協")
    ("2611202348" . "あさ診")
    ("2613100656" . "医誠会")
    ("2614101075" . "洛和会")
    ("2610204741" . "上京診")
    ("2610604080" . "川　端")
    ("2613344" . "洛　北")
    ("2610303667" . "京工場")))

(defstruct %cell x y content)

(defmacro fcolor (sheet &rest args)
  `(set-colorindex ,sheet ,@args :interior excel::xlpink))
(defmacro rcolor (sheet &rest args)
  `(set-colorindex ,sheet ,@args :interior excel::xlskyblue))
(defun num->col (num)
  (excel::number-to-col num))
(defun col->num (col)
  (excel::col-to-number col))

(defun row-number (list)
  (1+ (find-if-regexp->index "生.*年.*月.*日"
			     list)))

(defun code->index (code indexes)
  (util::vector-find-if->index
   (lambda (c) (and c (equal code (chc::code-> c))))
   indexes))

(defun now ()
  (let* ((n (local-time::timestamp+ (local-time::now) 9 :hour)))
    (format nil "~A~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
	    (local-time::timestamp-year n)
	    (local-time::timestamp-month n)
	    (local-time::timestamp-day n)
	    (local-time::timestamp-hour n)
	    (local-time::timestamp-minute n)
	    (local-time::timestamp-second n))))

(def-clojure sheet (l hash)
  ((insnumber	(cell l (2 2)))
   (created	(cell l (2 4)))
   (occurd      (cell l (4 4)))
   (hcode       (cell l (4 2)))
   (hname       (cell l (4 3)))
   (index	(coerce (nth 3 l) 'vector))
   (indexj	(nth 4 l))
   (indexf	(iter (for c :in-vector index)
		      (collect (gethash c hash) :into pot)
		      (finally (return (coerce pot 'vector)))))
   (body	(butlast (nthcdr 5 l)))
   (total	(last1 l))
   (bcol	(row-number indexj))
   (newname	(format nil "~A(~A)-~A-~A.xls"
			hcode
			(cdr (assoc hcode hospital-alist :test #'equal))
			(util::date-8 occurd)
			(now))))
  (:nth       (num)
	      (mapcar (lambda (line) (nth num line)) body))
  (:reload    (l)
	      (setf body (butlast (nthcdr 5 l))))
  (:how-old   (d)
	      (how-old d (nendo-end (nendo-year occurd))))
  (:newfile   (file)
	      (namestring (merge-pathnames (base-directory file)
					   newname)))
  (:code->nth (code)
	      (let ((n (code->index code indexf)))
		(mapcar (lambda (line) (nth n line)) body)))
  (:birthlist ()
	      (mapcar (lambda (line) (nth (1- bcol) line)) body)))

(defun reload! (contents sheet)
  (funcall contents :reload (ole sheet :usedrange :value)))

(defun sheet-contents (sheet hash)
  (sheet (ole sheet :usedrange :value) hash))

(defmacro value! (sheet y x post)
  `(progn
     (let ((pre (ole ,sheet :cells ,y ,x :value)))
       (ole ,sheet :cells ,y ,x :addcomment (write-to-string (or pre "")))
       (setf (slot-value (ole ,sheet :cells ,y ,x) :value)
	     ,post))))

(in-package #:check-formatta) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline splitter convert))

(defun splitter (formatta)
  (register-groups-bind (int f)
      ("(N+)\\.*(N*)" formatta)
    (values int f)))

(defun convert (formatta)
  (declare (optimize speed) (type simple-string formatta))
  (multiple-value-bind (int f) (splitter formatta)
    (declare (type string int))
    (if (string-null f)
	(format nil "^\\d{0,~A}$" (length int))
	(format nil "^\\d{0,~A}.\\d{0,~A}$"
		(length int)
		(length (the string f))))))

(defgeneric match (string val))
(defmethod  match ((formatta String) (val String))
  (scan (convert formatta) val))
(defmethod  match ((formatta String) (val Integer))
  (declare (ignorable val))
  (scan "^N+$" formatta))
(defmethod  match ((formatta String) (val Float))
  (match formatta (format nil "~A" (float val 0s0))))

(in-package :check-repval) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline %newline %in-order-p))
(define-condition codenumber-out-of-order () ())
(define-condition out-of-selection () ())
(defgeneric repair (obj val))
(defgeneric %number-value (obj val))

(defun %newline (string)
  (regex-replace-all "[
]" string ""))

(defun %in-order-p (cnum val)
  (declare (optimize speed)
	   (type CHECK-CODE:CODENUMBER cnum))
  (let ((n (etypecase val
	     (string (read-from-string val))
	     (t      val))))
    "不正な値が入力された場合(医誠会診療所の例では「*」が入力されていた)は、通した上で
最高値から1引いた数字を返す。repair-number-value参照"
    (handler-case
	(and (< (the fixnum (chc::min-> cnum)) (the fixnum n))
	     (> (the fixnum (chc::max-> cnum)) (the fixnum n)))
      (type-error (e) (declare (ignore e)) t))))

(defmethod %number-value ((c CHECK-CODE:CODENUMBER) (val SINGLE-FLOAT))
  (declare (optimize speed))
  (multiple-value-bind (int f)
      (chf::splitter (chc::formatta-> c))
    (declare (ignorable int))
    (let1 base (expt 10 (length f))
      (/ (round val (/ 1 base)) (if (eq base 1) 1 (float base))))))
(defmethod %number-value ((c CHECK-CODE:CODENUMBER) (val DOUBLE-FLOAT))
  (%number-value c (float val 0s0)))
(defmethod %number-value ((c CHECK-CODE:CODENUMBER) (val STRING))
  (if (string= "" val)
      ""
      (if (ppcre:scan "^[0-9\.]+$" val)
	  (%number-value c (read-from-string val))
	  (%number-value c (float (1- (car (chc::max-> c))) 0s0)))))
(defmethod %number-value ((c CHECK-CODE:CODENUMBER) (n NULL))
  nil)

(defmethod  repair ((c CHECK-CODE:CODENUMBER) val)
  (if (%in-order-p c val)
      (if (chf:match (chc::formatta-> c) val)
	  val
	  (%number-value c val))
      (error 'codenumber-out-of-order)))
(defmethod  repair ((c CHECK-CODE:CODECODE) (val STRING))
  (if (member val (chc::select-> c) :test #'equal)
      val
      (error 'out-of-selection)))
(defmethod  repair ((c CHECK-CODE:CODECODE) (val INTEGER))
  (repair c (write-to-string val)))
(defmethod  repair ((c CHECK-CODE:CODECODE) (val FLOAT))
  (repair c (truncate val)))
(defmethod  repair ((c CHECK-CODE:CODESTRING) (val INTEGER))
  (format nil "'~A" (to-zenkaku (write-to-string val))))
(defmethod  repair ((c CHECK-CODE:CODESTRING) (val FLOAT))
  (repair c (truncate val)))
(defmethod  repair ((c CHECK-CODE:CODESTRING) (val STRING))
  (let ((max (chc::formatta-> c)))
    (if (> (length val) max)
	(subseq val 0 (1- max))
	val)))
(defmethod  repair ((c CHECK-CODE:CODENUMBER) (val Null)) "")
(defmethod  repair ((c CHECK-CODE:CODESTRING) (val Null)) "")
(defmethod  repair ((c CHECK-CODE:CODECODE) (val Null)) "")

(in-package :check-body) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun equiv (val1 val2)
  (if (and (typep val1 'Float) (typep val2 'String))
      (eql (truncate val1) (read-from-string val2))))

(defun core (content-clojure)
  (iter (with ary = #[content-clojure :indexf])
	(for line :in #[content-clojure :body])
	(for row :upfrom 6)
	(appending (iter (for cell :in line)
			 (for col :upfrom 0)
			 (for code = (svref ary col))
			 (for post = (and code (chrv:repair code cell)))
			 (print (list row col cell))
			 (when (and code post cell (not (equal cell post))
				    (not (equiv cell post)))
			   (collect (list row col cell post)))))))

(defun color (sheet row column)
  (check:rcolor sheet ((excel::number-to-col (1+ column)) row)))

(defun number-format (sheet row column code)
  (if (typep code 'CHECK-CODE:CODENUMBER)
      (set-format sheet ((excel::number-to-col (1+ column)) row)
		  ;; (formatta-of code)は\"NN.N\"のような値を返す。Excelの表示形式は\"##.#\"である。
		  ;; Excelの表示形式について、
		  ;; 書式	対象	表示
		  ;; ##.##	1.4	1.4
		  ;; 00.00	1.4	01.40
		  ;; ##.##	123.456	123.46
		  ;; 00.00	123.456	123.46

		  ;; 「#」の場合は表示する数値が無い場合は無視されますが、「0」の場合は0が補って表示されます。またどちらの場合でも整数部分が書式に指定した文字数よりも多くてもそのまま表示されますが、小数点以下の部分は書式に指定した桁の位置で四捨五入されて表示されます。
		  (ppcre:regex-replace-all
		   "N" (chc::formatta-> code) "#"))))

(defun %value (sheet row column value)
  (check:value! sheet row (1+ column) value))

(defun repair (sheet content-clojure)
  (iter (with indexf = #[content-clojure :indexf])
  	(for (row col pre post) :in (core content-clojure))
  	(for code = (aref indexf col))
  	(ole sheet :cells row (1+ col) :select)
  	(color         sheet row col)
  	(number-format sheet row col code)
  	(%value        sheet row col post))
  (check::reload! content-clojure sheet))

(in-package :check-base) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %name-repair (sheet row name)
  (register-groups-bind (fam fir) ("([^ 　]+)[ 　]+([^ 　]+)" name)
    (check:value! sheet row 3 (format nil "~A　~A" fam fir))
    (check:rcolor sheet (:c row))))

(defun name-repair (sheet)
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (chc::xls-checker-format "氏名欄の修正~%")
  (force-output)
  (let ((val (value sheet (:c 6) (:c (lastrow sheet :y 5 :x 3)))))
    (iter (for line :in (if (listp val) val (list val)))
  	  (for row :upfrom 6)
  	  (cond
  	    ;; 正常なもの
  	    ((scan "([^ 　]+)　([^ 　]+)" line)
  	     nil)
  	    ;; 修正可能なもの
  	    ((scan ".+[ 　]+.+" line)
  	     (%name-repair sheet row line))
  	    ;; 修正不可のもの。目視で確認
  	    (t
  	     (check:fcolor sheet (:c row)))))))

(defun %bformat (sheet row column)
  (set-format sheet (column row) "yyyy/mm/dd"))

(defun %bcore (sheet row column value)
  (excel::value! sheet (column row)
		 (util::normal-date-string value))
  (check:rcolor sheet (column row)))

(defun birthday-repair (sheet contents)
  (let* ((colnum #[contents :bcol])
	 (col (excel::number-to-col colnum))
	 (lr  (lastrow sheet :y 5 :x colnum))
	 (val (value sheet (col 6) (col lr))))
    (chc::xls-checker-format "生年月日欄の修正~%")
    (force-output)
    (optima:match val
      ((TYPE LIST)
       (iter (for (line) :in val)
	  (for row :upfrom 6)
	  (%bformat sheet row col)
	  (cond
	    ((typep line 'dt:date-time)
	     (next-iteration))
	    (t
	     (%bcore sheet row col line)))))
      ((TYPE ATOM)
       (%bformat sheet 6 col)
       (%bcore sheet 6 col val)))))

(defun insurance-repair (sheet)
  (chc::xls-checker-format "保険者欄の修正~%")
  (force-output)
  (set-format sheet (:b 2) (:c 2) "@")
  (check:value! sheet 2 2 "00263129")
  (check:value! sheet 2 3 "京都建築国民健康保険組合"))

(in-package :check-hba1c) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %width (sheet)
  (mapcar (lambda (n) (slot-value
		       (ole sheet :range (format nil "~A:~:*~A" (excel::number-to-col n)))
		       :ColumnWidth))
	  (iota :from 1 :to 100)))

(defun %row (list)
  (iter (for code :in (mapcar #'car list))
	(for c :upfrom 3)
	(if (equal "3D045000001906202" code)
	    (leave (1+ c)))))

(defclass SHEET ()
  ((book    :initarg :book
	    :accessor book->)
   (sheet   :accessor sheet->)
   (width   :accessor width->)
   (name    :initarg :name
	    :accessor name->)
   (row     :accessor row->)
   (lastrow :accessor lastrow->)
   (index   :accessor index->)))

(defmethod initialize-instance :after
    ((m SHEET) &rest args)
  (declare (ignorable args))
  (with-slots (sheet width name row lastrow index book) m
    (setq sheet		(ole book :WorkSheets :Item name)
	  width		(%width sheet)
	  ;; index		
	  lastrow	(lastrow sheet :y 2 :x 30)
	  row		(%row (value sheet (:ad 3) (:ad lastrow))))))

(defun has-nbps? (contents)
  (vector-find-if
   (lambda (el)
     (and el (equal (chc::code-> el) "3D046000001906202")))
   #[contents :indexf]))

(defun has-jds? (contents)
  (iter (for el :in-vector #[contents :indexf])
	(for c :upfrom 0)
	(if (and el (ppcre:scan "^3D045.+" (chc::code-> el)))
	    (leave c))
	(finally (return nil))))

(defun index-repair (sheet col)
  (check:value! sheet 4 (1+ col) "3D046000001906202"))

(defun %insert-blank-row (sheet row)
  (ole sheet :range (format nil "~A:~:*~A" row) :insert))

(defun %shrink-width (sheet)
  (set-width sheet (format nil "A:~A" (excel::number-to-col 100))
	     1))

(defun %put-value (sheet row)
  (excel::value! sheet (:a row) (:bb row)
		 (car (value sheet (:a (1- row)) (:bb (1- row))))))

(defun %put-value2 (sheet row)
  (excel::value! sheet (:ae (1- row)) ""))

(defun %expand-width (sheet width)
  (iter (for i :from 0 :to 100)
	  (for col = (excel::number-to-col (1+ i)))
	  (set-width sheet col (nth i width))))

(defun %put-index (sheet row)
  (excel::value! sheet (:ad row) "3D046000001906202"))

(defun insert (excel-sheet)
  (declare (type SHEET excel-sheet))
  (with-slots (row width sheet) excel-sheet
    (%INSERT-BLANK-ROW sheet row)
    (%SHRINK-WIDTH sheet)
    (%PUT-VALUE sheet row)
    (%PUT-VALUE2 sheet row)
    (%EXPAND-WIDTH sheet width)
    (%PUT-INDEX sheet row)))

(defgeneric %calculate-value (val))
(defmethod %calculate-value ((f Number))
  (float (/ (round (* 10 (+ f 0.4))) 10) 0d0))
(defmethod %calculate-value ((s String))
  (%calculate-value (read-from-string s)))
(defmethod %calculate-value ((n Null))
  "")

(defun %input-values (contents column)
  (mapcar (lambda (line) (nth column line))
	  #[contents :body]))

(defun %repaired-values (contents column)
  (mapcar #'%calculate-value
	  (%input-values contents column)))
;; 要確認
(defun repair-values (sheet contents col)
  (decide-range-value sheet
		      (%repaired-values contents col)
		      :start-row 6
		      :start-column (1+ col)))

(defun repair (book sheet contents)
  (declare (optimize debug safety))
  (if (not (has-nbps? contents))
      (awhen (has-jds? contents)
	(chc::xls-checker-format "Hba1c欄の修正~%")
	(force-output)
	;; (1) 入力シートのインデックスを直す
	(index-repair sheet it)
	;; (2) コード一覧に3D046000001906202を付け足す
	;; (hba1c-repair/code-sheet book)
	(let ((obj (make-instance 'SHEET
				  :book book
				  :name "健診項目コード表")))
	  (insert obj))
	;; (3) 入力された値をJDS値からNGSP値に直す。JDS値に0.4%を足した数字がNGSP値といわれている。
	;; (repair-values sheet contents it)
	)))

(in-package :check-relative) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %indexes ()
  (mapcar (lambda (outer) (mapcar #'car outer))
	  kcsv::relative))
(define-condition NOTHING-WRITTEN () ())
(define-condition WRITTEN2 () ())
(define-condition SOMETHING-WRONG () ())

(defun %string-none? (string)
  (ppcre:scan "(異|特).*[無な]し" string))
(defgeneric match (arg1 arg2))
(defmethod  match ((n NULL) (f NUMBER))
  (if (eq 2 (truncate f))
      t
      (error 'NOTHING-WRITTEN)))
(defmethod  match ((n NULL) (s STRING))
  (if (equal "2" s)
      t
      (error 'NOTHING-WRITTEN)))
(defmethod  match ((n1 NULL) (n2 NULL))
  (error 'NOTHING-WRITTEN))
(defmethod  match ((s STRING) (f NUMBER))
  (if (%string-none? s)
      (match nil f)
      (if (eq 1 (truncate f))
	  t
	  (error 'WRITTEN2))))
(defmethod  match ((s1 STRING) (s2 STRING))
  (if (string-null s1)
      (match nil s2)
      (if (%string-none? s1)
	  (match nil s2)
	  (match s1 (read-from-string s2)))))
(defmethod  match ((s STRING) (n2 NULL))
  (if (%string-none? s)
      (error 'NOTHING-WRITTEN)
      (error 'WRITTEN2)))
(defmethod  match (s n)
  (error 'SOMETHING-WRONG))

(defmacro match-case (form &rest clause)
  `(handler-case ,form
     ,@(mapcar
	(lambda (l)
	  (destructuring-bind (condition binding list) l
	    `(,(intern (format nil "~A" condition))
	       ,binding
	       (declare (ignorable ,@binding))
	       (collect (list o b n) :into ,list))))
	clause)))

(defun %operate-nothing-written (sheet list val)
  (when list
    (iter (for (o b row) :in list)
	  (for ocol = (check:num->col (1+ o)))
	  (for bcol = (check:num->col (1+ b)))
	  (check:rcolor sheet (ocol row))
	  (check:rcolor sheet (bcol row))
	  (check:value! sheet row (1+ b) val))))

(defun %operate-something (sheet list)
  (when list
    (iter (for (o b row) :in list)
	  (for ocol = (check:num->col (1+ o)))
	  (for bcol = (check:num->col (1+ b)))
	  (check:fcolor sheet (ocol row))
	  (check:fcolor sheet (bcol row)))))

(defun %classify (sheet contents opinion boolean)
  (iter (with o = (check:code->index opinion #[contents :indexf]))
	(with b = (check:code->index boolean #[contents :indexf]))
	(with olist = #[contents :code->nth opinion])
	(with blist = #[contents :code->nth boolean])
	(with birth = #[contents :birthlist])
	(for birthday :in birth)
	(for n :upfrom 6)
	(for op = (nth (- n 6) olist))
	(for bl = (nth (- n 6) blist))
	(if (kensin::kensin-year? #[contents :how-old birthday])
	    (match-case
	     (match op bl)
	     (:nothing-written (sym) nothing)
	     (:written2        (sym) written)
	     (:something-wrong (sym) something)))
	(finally (progn
		   (%operate-nothing-written sheet nothing 2)
		   (%operate-nothing-written sheet written 1)
		   (%operate-something sheet something)))))

(defun repair (sheet contents)
  (chc::xls-checker-format "相関的修正~%")
  (force-output)
  (iter (for (opinion boolean) :in (%indexes))
	(%classify sheet contents opinion boolean))
  (check:reload! contents sheet))

(in-package :check-must) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %indexes (contents)
  (mapcar (lambda (code)
	    (util::vector-find-if->index
	     (lambda (c) (equal code c))
	     #[contents :index]))
	  (mapcar #'car kcsv::must)))

(defun %line-collect (line row indexes)
  (iter (for n :in indexes)
	(for val = (nth n line))
	(if (or (string-null val) (not val))
	    (collect (check::make-%cell :x (1+ n) :y row :content val)))))

(defun %collect (contents)
  (iter (with indexes  = (%indexes contents))
	(with birthday = #[contents :birthlist])
	(for line :in #[contents :body])
	(for row :upfrom 6)
	(when (kensin::kensin-year?
	       #[contents :how-old (nth (- row 6) birthday)])
	  (appending (%line-collect line row indexes)))))

(defun repair (sheet contents)
  (chc::xls-checker-format "必須項目の欠損確認~%")
  (force-output)
  (iter (for cell :in (%collect contents))
	(check:fcolor sheet ((check:num->col (check::%cell-x cell))
			     (check::%cell-y cell)))))

(in-package :check-available) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter %indexes
  '(("9N701000000000011" . "服薬１（血圧）")
    ("9N706000000000011" . "服薬２（血糖）")
    ("9N711000000000011" . "服薬３（脂質）")
    ("9N736000000000011" . "喫煙")))

(defun %indexes (contents)
  (mapcar
   (lambda (id)
     (util::vector-find-if->index
      (lambda (c) (equal c id))
      #[contents :index]))
   (mapcar #'car %indexes)))

(defun %collect (contents)
  (iter (with ns = (%INDEXES contents))
	(with birthday = #[contents :birthlist])
	(for line :in #[contents :body])
	(for row :upfrom 6)
	(if (kensin::kensin-year?
	     #[contents :how-old (nth (- row 6) birthday)])
	    (appending (chm::%line-collect line row ns)))))

(defun repair (sheet contents)
  (chc::xls-checker-format "必須項目の修正~%")
  (force-output)
  (iter (for m :in (%COLLECT contents))
	(check:value! sheet (check::%cell-y m) (check::%cell-x m) "2")
	(check:rcolor sheet ((check:num->col (check::%cell-x m))
		       (check::%cell-y m))))
  (check:reload! contents sheet))

(in-package :check-either);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter %indexes
  (mapcar #'cdr kcsv::either))

(defun %indexes (contents)
  (mapcar
   #'compact
   (tree-map (lambda (c)
	       (util::vector-find-if->index
		(lambda (code) (equal c code))
		#[contents :index]))
	     %indexes)))

(defun %objectize (line gen row)
  (tree-map
   (lambda (n)
     (check::make-%cell :x (1+ n) :y row :content (nth n line)))
   gen))

(defun %color (sheet leaf)
  (with-slots (check::x check::y) leaf
    (setf (slot-value (ole sheet
			   :cells check::y check::x
			   :interior)
		      :ColorIndex)
	  excel::xlpink)))

(defun %check (sheet tree)
  (iter (for leaf :in tree)
	(unless (any #'check::%cell-content leaf)
	  (progn
	    (mapc (lambda (l) (%color sheet l)) leaf)
	    (appending leaf)))))

(defun repair (sheet contents)
  (chc::xls-checker-format "選択的修正~%")
  (force-output)
  (iter (for gen = (%indexes contents))
  	(with birthday = #[contents :birthlist])
  	(for line :in #[contents :body])
  	(for row :upfrom 6)
  	(if (kensin::kensin-year?
  	     #[contents :how-old (nth (- row 6) birthday)])
  	    (appending
  	     (%check sheet (%objectize line gen row))))))

(in-package :check-hanzen) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline %check-string %check-number %check))

(format t "check-string")
(defun %check-string (sheet col contents)
  (iter (for cell :in contents)
	(for row :upfrom 6)
	(declare (type fixnum row col))
	(optima:match cell
	  ((type NULL)
	   (next-iteration))
	  ((type STRING)
	   (let ((post (to-zenkaku (the string cell))))
	     (declare (type string post))
	     (if (equal cell post)
		 (next-iteration)
		 (progn
		   (check:value! sheet row col post)
		   (check:rcolor sheet ((check:num->col col) row))))))
	  ((type NUMBER)
	   (let ((post (& to-zenkaku write-to-string truncate cell)))
	     (check:value! sheet row col post)
	     (check:rcolor sheet ((check:num->col col) row)))))))

(format t "check-number")
(defun %check-number (sheet col contents)
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (iter (for cell :in contents)
	(for row :upfrom 6)
	(declare (type fixnum row))
	(optima:match cell
	  ((type NULL)
	   (next-iteration))
	  ((type NUMBER)
	   (next-iteration))
	  ((type STRING)
	   (when (not (equal (to-hankaku cell) cell))
	     (check:value! sheet row col (& read-from-string to-hankaku cell))
	     (check:rcolor sheet ((check:num->col col) row)))))))

(format t "check")
(defun %check (sheet col index contents)
  (typecase index
    (CHECK-CODE:CODESTRING
     (%check-string sheet col contents))
    (CHECK-CODE:CODENUMBER
     (%check-number sheet col contents))
    (CHECK-CODE:CODECODE
     (%check-number sheet col contents))))

(format t "repair")
(defun repair (sheet contents)
  ;; (declare (optimize speed))
  (iter (with index = #[contents :indexf])
	(declare (type array index))
	(for i :from 0 :to (1- (length index)))
	(for id = (aref index i))
	(declare (type fixnum i))
	(if id
	    (collect (%check sheet (1+ i) id #[contents :nth i]))
	    (next-iteration))))

(in-package :check)

(defun %core (excel-application filename &key (close nil))
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((f (namestring filename)))
    (with-excel-book (excel-application b f :close close :debugger t)
      (let* ((sheet    (ole b :worksheets :item "健診結果入力シート"))
	     (hash     (check-code:info b))
	     (contents (sheet-contents sheet hash))
	     (newfile  #[contents :newfile f]))
	;; (check-body:repair sheet contents)
	(check-base:name-repair sheet)
	(check-base:birthday-repair sheet contents)
	(check-base:insurance-repair sheet)
	(check-hba1c:repair b sheet contents)
	(check-relative:repair sheet contents)
	(check-available:repair sheet contents)
	(check-must:repair sheet contents)
	(check-either:repair sheet contents)
	(check-hanzen:repair sheet contents)
	(excel::save-book b newfile :xls)
	(format t "処理が終わりました。~%")
	newfile))))

(defun parse-filename (path)
  (util::regex-replace-alist
   path
   '(("\\\\" . "/")
     ("\"" . ""))))

(defun file-main (file &key (quit nil) (close nil))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (with-excel (app :visible t :quit quit :debugger t)
    (let* ((truefile (parse-filename file))
	   (newfile (%core app truefile :close close))
	   (dfile   (merge-pathnames #P"d:/特定健診結果データ/"
	   			     newfile)))
      (unless (cl-fad:file-exists-p dfile)
      	(cl-fad:copy-file newfile dfile)))))

(defun directory-main (directory)
  (with-excel (app :visible t :quit nil :debugger t)
  (mapc
   (lambda (file) (%core app file :close t))
   (directory-list directory :type "xls"))))
