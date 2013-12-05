(in-package :kensin/csv)

(declaim (inline string-null))

(defparameter topdir
  #P"f:/kcsv/")

(defparameter %directory #P"d:/特定健診結果データ/連合会に送付するもの/")
(defparameter %directory2 #P"d:/特定健診結果データ/連合会に送付するもの/2013/")
(defparameter csv-output-type
  :new		; 300.csvを修正したものが301.csvとなるように出力
  ;:exchange	; 300.csvを修正したものが300.csvとなり、元の300.csvを300.csv.bakとして保存
  )

(defvar csv-filename-regexp "00263129_FKAC522_(\\d{8})_(\\d{3})")

(defparameter relative-error-regexp "(異常)*.*[な無]し")

(defvar must
  '(("9N001000000000001" . "身長")
    ("9N006000000000001" . "体重")
    ("9N011000000000001" . "ＢＭＩ")
    ("9N056000000000011" . "既往歴")
    ("9N061000000000011" . "自覚症状")
    ("9N066000000000011" . "他覚症状")
    ("9N511000000000049" . "医師の診断（判定）")
    ("9N516000000000049" . "健康診断を実施した医師の氏名")
    ("9N701000000000011" . "服薬１（血圧）")
    ("9N706000000000011" . "服薬２（血糖）")
    ("9N711000000000011" . "服薬３（脂質）")
    ("9N736000000000011" . "喫煙")))

(defvar must-title
  (map 'vector #'cdr must))

(defvar must-repairable
  '("9N011000000000001" "9N056000000000011" "9N061000000000011"
    "9N066000000000011" "9N701000000000011" "9N706000000000011"
    "9N711000000000011" "9N736000000000011"))

(defvar either
  '(("腹囲系"     "9N021000000000001" "9N016160100000001" "9N016160200000001" "9N016160300000001")
    ("収縮期血圧" "9A755000000000001" "9A752000000000001" "9A751000000000001")
    ("拡張期血圧" "9A765000000000001" "9A762000000000001" "9A761000000000001")
    ("中性脂肪"   "3F015000002327101" "3F015000002327201" "3F015000002399901")
    ("HDLコレステロール" "3F070000002327101" "3F070000002327201" "3F070000002399901")
    ("LDLコレステロール" "3F077000002327101" "3F077000002327201" "3F077000002399901")
    ("GOT" "3B035000002327201" "3B035000002399901")
    ("GPT" "3B045000002327201" "3B045000002399901")
    ("γ-GT(γ-GTP)" "3B090000002327101" "3B090000002399901")
    ("空腹時血糖" "3D010000001926101" "3D010000002227101" "3D010000001927201" "3D010000001999901" "3D045000001906202" "3D045000001920402" "3D045000001927102" "3D045000001999902" "3D046000001906202")))

(defvar either-title
  (map 'vector #'car either))

(defvar relative
  '((("9N056160400000049" . "（具体的な既往歴）")
     ("9N056000000000011" . "既往歴"))
    (("9N061160800000049" . "（所見）")
     ("9N061000000000011" . "自覚症状"))
    (("9N066160800000049" . "（所見）")
     ("9N066000000000011" . "他覚症状"))))

(defvar relative-title
  (map 'vector (compose #'cdr #'second) relative))

(defun title ()
  (append (mapcar #'cdr must)
	  (mapcar #'car either)
	  (mapcar (compose #'cdr #'second) relative)))

(defstruct csv
  pathname created occurd hcode hname index body errors)

(defmacro with-csv-slots (instance &body body)
  `(with-slots (pathname created occurd hcode hname index body errors) ,instance
     ,@body))

(defmacro cells (sheet row col)
  `(nth (1- ,col) (nth (1- ,row) ,sheet)))

(defun make-either-error (baselist)
  (iter (for kind :in either)
	(collect (iter (for id :in kind)
		       (unless (first-time-p)
			 (aif (index id baselist)
			      (collect (1+ it))))))))

(defun make-num2id-hash (maine)
  (iter (for id :in maine)
	(for col :upfrom 1)
	(shash id :condition t
	       :key   (lambda (_) (declare (ignorable _)) col)
	       :value (lambda (i) (list i (code->title i))))))

(util::def-clojure index-clojure (list)
  ((maine	list)
   (mainj	(mapcar #'code->title list))
   (num2id-hash (make-num2id-hash maine))
   (merr	(mapcar (lambda (cl) (1+ (index (car cl) maine)))
			must))
   (eerr	(make-either-error maine))
   (rerr	(mapcar (lambda (cl) (mapcar (lambda (i) (aif (index (car i) maine) (1+ it)))
					     cl))
			relative)))
  (:num2id (num) (gethash num num2id-hash))
  (:hanzen (num) (gethash (first (gethash num num2id-hash))
			  hanzen-hash))
  (:maine! (list)
	   (setf maine list)
	   (setf num2id-hash (make-num2id-hash maine))))

(defstruct cell x contents fix-value)

(defstruct line name bango birthday number contents row shibu repair
	   merr eerr rerr errors)

(defun make-contents (list row)
  (declare (type list list) (type fixnum row) (optimize (speed 3) (safety 0))
	   (ignorable row))
  (iter (for c :in (nthcdr 6 list))
	(for col :upfrom 7)
	(declare (type fixnum col))
	(collect (make-cell :x (the fixnum col)
			    :contents c
			    :fix-value nil))))

(defun make-shibu (number bango)
  (if-match-bind
      ((char 3) (scode1 (string 2)) (+ char) (last))
      (the string number)
    (if (equal "85" scode1)
	(to-hankaku (subseq bango 0 2))
	scode1)))

(defun line-search (num contents)
  (declare (type integer num) (type list contents)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (find-if (lambda (cell)
	     (eq (the integer (cell-x cell)) (the integer num)))
	   (the list contents)))

(defmethod must-error ((l line) index)
  (mapcar (lambda (num)
	    (let1 cell (line-search num (line-contents l))
	      (if (or (not cell)
		      (string-null (cell-contents cell))) 1 0)))
	  index))

(defun numbers-to-contents (numbers line)
  ;; (declare (type list numbers) (type line line)
  ;; 	   (optimize (speed 3) (safety 0) (debug 0)))
  ;; (the list
  ;;      (mapcar
  ;; 	(lambda (num) (aif (the (or cell nil) (line-search num line))
  ;; 			   (the string (cell-contents it)) nil))
  ;; 	(the list numbers)))
  (mapcar (lambda (num) (aif (line-search num line)
			     (cell-contents it) nil))
	  numbers))

(defun any-string-fill (string-list)
  (find-if #'string-not-null string-list))

(defmethod either-error ((l line) index)
  (iter (with lcontents = (line-contents l))
	(for numlist :in index)
	(collect (if (any-string-fill (numbers-to-contents numlist lcontents))
		     0 1))))

(defun relative-error-p (string-pair)
  ;; (destructuring-bind (opinion boolean) string-pair
  ;;   (or (string-null boolean)
  ;; 	(and (string= "1" boolean)
  ;; 	     (string-null opinion))
  ;; 	(and (string= "2" boolean)
  ;; 	     (and (string-not-null opinion)
  ;; 		  (not (ppcre:scan relative-error-regexp opinion))))))
  ;; (string boolean)
  (optima:match string-pair
    ((LIST _ nil)	t)
    ((LIST nil "2")     nil)
    ((LIST str "1")
     (if (string-null str) t nil))
    ((LIST str "2")
     (if (and (string-not-null str)
	      (not (ppcre:scan relative-error-regexp str)))
	 t nil))))

(defmethod relative-error ((l line) index)
  (iter (with lcontents = (line-contents l))
	(for kind :in index)
	(collect (if (relative-error-p (numbers-to-contents kind lcontents))
		     1 0))))

(defun body-error-calculate (body)
  (iter (with result = nil)
	(for line :in body)
	(setq result
	      (if (first-time-p)
		  (line-errors line)
		  (mapcar #'+ result (line-errors line))))
	(finally (return result))))


(defun create-line (list num index)
  (declare (type list list) (type fixnum num) (ftype function index)
	   (optimize (speed 0) (safety 3) (debug 3)))
  (let1 obj (make-line)
    (declare (type line obj))
    (with-slots (name bango birthday number contents row shibu repair
		      merr eerr rerr errors) obj
      (setq row		num
	    name	(the string (third list))
	    bango	(the string (fourth list))
	    birthday	(the string (normal-date->string
				     (strdt (fifth list))))
	    number	(the string (sixth list))
	    contents	(the list (make-contents list row))
	    shibu	(make-shibu number bango)
	    repair	nil
	    merr	(must-error obj (funcall (the function index) :merr))
	    eerr	(either-error obj (funcall (the function index) :eerr))
	    rerr	(relative-error obj (funcall (the function index) :rerr))
	    errors	(append merr eerr rerr)))
    obj))

;;; #P"f:/kcsv/00263129_FKAC522_20130119_010.csv"
;;; #P"f:/kcsv2/00263129_FKAC522_20130619_010.csv"
;;; #P"f:/zip/"
(defun create-csv (pathname)
  (declare (type pathname pathname) (optimize (speed 0) (safety 3) (debug 3)))
  (let1 obj (make-csv :pathname pathname)
    (let1 init (util:csv-read-to-list pathname :code :SJIS)
      (with-csv-slots obj
	(setq hcode	(cells init 2 2)
	      hname	(car (gethash hcode kensin:hospital-hash))
	      occurd	(cells init 2 4)
	      created	(cells init 1 4)
	      index	(index-clojure (second init))
	      body	(iter (for line :in init)
	      		      (for row :upfrom 1)
	      		      (if (equal (car line) "5")
	      			  (collect (create-line line row index))))
	      errors	(body-error-calculate body)))
      obj)))

;;; 実装方針
;;; (1) 以下のものは修正する。
;;;     ・半角全角
;;;     ・Boolean値(既往歴・自覚症状・他覚症状・喫煙・服薬1〜3・既往歴1〜3)
;;; (2) 修正できない実値に遭遇すると、エラーを出してとまる。
;;;     (conditionの使用)  そこで、
;;;     ・修正するか、
;;;     ・無視して(空白のままで開けておいて)続行するか、
;;;     ・その場で処理を中止するかを選ぶ
;;; (3) ファイル全体でエラーがない場合はnilを返す。
;;; (4) エラーがある場合は、エラーがある行のオブジェクトから成るリストを返し、
;;;     xmlを吐く。
;;;     また、
;;;     ・修正したCSVを元のCSV名で出力し、元のCSVを別の名前で保存し直すか、
;;;     ・元のCSVをそのままの名前で残しておき、修正したCSVを別名で出力するか
;;;     を選ぶ。
(define-condition kensin-condition (simple-condition)
  ((name  :initarg :name  :reader name-of)
   (row   :initarg :row   :reader row-of)
   (title :initarg :title :reader title-of)
   (col   :initarg :col   :reader col-of)))

(define-condition kensin-error (kensin-condition)
  ())

(define-condition kensin-either-error (kensin-error)
  ())

;; (defparameter testl '((2 3) ("2" "3")))
;; (iter (for line :in testl)
;;       (collect (iter (for cell :in line)
;; 	    (restart-case (collect (/ cell 2))
;; 	      (store (new-value)
;; 		:interactive (lambda ()
;; 			       (format t "new value: ")
;; 			       (list (read)))
;; 		(collect (/ new-value 2)))))))

;; (restart-case (error 'kensin-either-error :format-control "hgoe")
;;   (ignore ))

(define-condition kensin-warning (simple-warning kensin-condition)
  ())

(define-condition kensin-either-warning (kensin-warning)
  ())

(define-condition kensin-hankaku-zenkaku-warning (kensin-warning)
  ((hanzen :initarg :hanzen :reader hz-of)
   (pre    :initarg :pre    :reader pre-of)
   (post   :initarg :post   :reader post-of)))

(define-condition kensin-relative-warning (kensin-warning)
  ((boolean  :initarg :boolean  :reader b-of)
   (opinion  :initarg :opinion  :reader o-of)
   (repaired :initarg :repaired :reader repaired-of)))

(define-condition kensin-must-warning (kensin-warning)
  ((repaired :initarg :repaired :accessor repaired-of)))

(defgeneric repair (arg))

(defun repair-hanzen (index line)
  (declare (type line line) (optimize (speed 3) (safety 0) (debug 0)))
  (optima:match line
    ((line contents)
     (iter (for cell :in contents)
	   (with-slots (contents fix-value) cell
	     (let ((post #[(if (eq #[index :hanzen (cell-x cell)] 'hankaku)
	     		       #'to-hankaku #'to-zenkaku) contents]))
	       (if (equal post contents)
	     	   (next-iteration)
	     	   (let1 obj (make-instance 'kensin-hankaku-zenkaku-warning
					     :pre    contents
					     :post   post
					     :hanzen #[index :hanzen (cell-x cell)]
					     :col    (cell-x cell)
					     :title  #[index :num2id (cell-x cell)]
					     :row    (line-row line)
					     :name   (line-name line))
		     (warn 'kensin-hankaku-zenkaku-warning
			   :format-control "~A(~A行目)の~A(~A列)を~Aから~Aに修正しました(~A => ~A)。"
			   :format-arguments (list (name-of obj)
						   (row-of obj)
						   (second (title-of obj))
						   (col-of obj)
						   (if (eq (hz-of obj) 'hankaku) "全角" "半角")
						   (if (eq (hz-of obj) 'hankaku) "半角" "全角")
						   (pre-of obj)
						   (post-of obj)))
	     	     (collect obj :into pot)
	     	     (setf fix-value post)))))
	   (finally (return pot))))))

(defun any (pred list)
  (cond
    ((null list) nil)
    ((funcall pred (car list)) (values (car list) t))
    (t (any pred (cdr list)))))

(defun make-either-warning-instance (index counter line)
  (let1 etitle #[index :eerr]
	(make-instance 'kensin-either-warning
			 :col     (sort2 (nth counter etitle) < identity)
			 :row     (line-row line)
			 :name    (line-name line)
			 :title   (svref either-title counter)
			 ;; (if (member title must-repairable :test #'equal) "2" nil)
			 )))

(defun make-either-error-instance (index counter line)
  (let1 etitle #[index :eerr]
	(make-instance 'kensin-either-error
		       :col     (sort2 (nth counter etitle) < identity)
		       :row     (line-row line)
		       :name    (line-name line)
		       :title   (svref either-title counter))))

(defun warn-either (instance)
  (warn 'kensin-either-warning
	:format-control "(fatal) ~A(~A行目)の\"~A\"(~{~A~^,~}列目)が欠損しています。"
	:format-arguments (list (name-of  instance)
				(row-of   instance)
				(title-of instance)
				(col-of   instance))))

(defmacro raise-warn-either (index i line)
  (let ((object (gensym)))
  `(let1 ,object (make-either-warning-instance ,index ,i ,line)
     (warn-either ,object)
     (collect ,object :into pot))))

(defun error-either (instance)
  (error 'kensin-either-error
	 :format-control "~A(~A行目)の\"~A\"(~{~A~^,~}列目)が欠損しています。"
	 :format-arguments (list (name-of  instance)
				 (row-of   instance)
				 (title-of instance)
				 (col-of   instance))))

(defun repair-either (index line &key (type :warn))
  (iter (for element :in (line-eerr line))
	(for i :upfrom 0)
	(if (eq element 1)
	    (case type
	      (:warn  (raise-warn-either index i line))
	      (:error nil))
	    (next-iteration))
	(finally (return pot))))

(defun make-relative-warning-instance (index counter line)
  (let* ((rtitle  (funcall index :rerr))
	 (boolean (line-search (second (nth counter rtitle))
			       (line-contents line))))
    (make-instance 'kensin-relative-warning
		   :col (nth counter rtitle)
		   :row (line-row line)
		   :name (line-name line)
		   :title (svref relative-title counter)
		   :opinion (line-search (first (nth counter rtitle))
					 (line-contents line))
		   :boolean boolean
		   :repaired (if (equal (cell-contents boolean) "1") "2" "1"))))

(defun warn-relative (instance)
  (warn 'kensin-relative-warning
	:format-control "~A(~A行目)の~A(~A列目)が誤っていたため(~A: ~A -> ~A)修正しました。"
	:format-arguments (list (name-of instance)
				(row-of instance)
				(title-of instance)
				(first (col-of instance))
				(cell-contents (b-of instance))
				(cell-contents (o-of instance))
				(repaired-of instance))))

(defun repair-relative (index line)
  (iter (for element :in (line-rerr line))
	(for i :upfrom 0)
	(if (eq element 1)
	    (let1 obj (make-relative-warning-instance index i line)
	      (warn-relative obj)
	      (setf (cell-fix-value (b-of obj)) (repaired-of obj))
	      (collect obj :into pot))
	    (next-iteration))
	(finally (return pot))))

(defun make-must-warning-instance (index counter line)
  (let* ((mtitle (funcall index :merr))
	 (col    (nth counter mtitle)))
    (make-instance 'kensin-must-warning
		   :col      col
		   :row      (line-row line)
		   :name     (line-name line)
		   :title    (svref must-title counter)
		   :repaired (if (member (car (funcall index :num2id col))
					 must-repairable
					 :test #'equal)
				 "2" nil))))

(defun mapfn (function-list target)
  (mapcar (lambda (f) (funcall (symbol-function f) target))
	  function-list))

(defun warn-must (instance)
  (if (repaired-of instance)
      (warn 'kensin-must-warning
	    :format-control "~A(~A行目)の~A(~A列目)が欠損していたので、修正しました。(=> \"2\")"
	    :format-arguments (mapfn '(name-of row-of title-of col-of)
				     instance))
      (warn 'kensin-must-warning
	    :format-control "(fatal) ~A(~A行目)の~A(~A列目)が欠損しています。"
	    :format-arguments (mapfn '(name-of row-of title-of col-of)
				     instance))))

(defun repair-must (index line)
  (iter (with merr = (funcall index :merr))
	(for element :in (line-merr line))
	(for i :upfrom 0)
	(for number = (nth i merr))
	(for id = (car #[index :num2id number]))
	(if (eq element 1)
	    (let1 obj (make-must-warning-instance index i line)
	      (warn-must obj)
	      (if (member (car #[index :num2id number]) must-repairable :test #'equal)
		  (with-slots (fix-value) (line-search number (line-contents line))
		    (setf (repaired-of obj) "2")
		    (setf fix-value "2")
		    (collect obj)))))))

;; #P"f:/kcsv2/00263129_FKAC522_20130618_020.csv"
(defun repaired-multiple-values (csv-instance)
  (optima:match csv-instance
    ((csv body index)
     (iter (for line :in body)
	   (appending (repair-relative index line) :into r)
	   (appending (repair-either index line) :into e)
	   (appending (repair-must index line) :into m)
	   (appending (repair-hanzen index line) :into h)
	   (finally (return (values r m e h)))))))

(defmacro xml-output-relative (relative-errors)
  (let ((item (gensym)))
    `(cl-who:htm
      (:relative-error
       (loop
	  :for ,item
	  :in  ,relative-errors
	  :do  (cl-who:htm (:item :row (row-of ,item) :title (title-of ,item)
				  (:name (cl-who:str (name-of ,item)))
				  (:opinion :column (first (col-of ,item))
					    (:pre (cl-who:str (cell-contents (o-of ,item))))
					    (:post (cl-who:str (cell-fix-value (o-of ,item)))))
				  (:boolean :column (second (col-of ,item))
					    (:pre (cl-who:str (cell-contents (b-of ,item))))
					    (:post (cl-who:str (cell-fix-value (b-of ,item))))))))))))

(defmacro xml-output-must (must-errors)
  (let ((item (gensym)))
    `(cl-who:htm
      (:must-error
       (loop
	  :for ,item
	  :in  ,must-errors
	  :do  (cl-who:htm (:item :row (row-of ,item) :title (title-of ,item)
				  (:name     (cl-who:str (name-of ,item)))
				  (:repaired (cl-who:str (repaired-of ,item))))))))))

(defmacro xml-output-either (either-errors)
  (let ((item (gensym)))
    `(cl-who:htm
      (:either-error
       (loop
	  :for ,item
	  :in  ,either-errors
	  :do  (cl-who:htm (:item :row (row-of ,item)
				  :title (title-of ,item)
				  (loop
				     :for c :in (col-of ,item)
				     :do (cl-who:htm (:column (cl-who:str c))))
				  (:name     (cl-who:str (name-of ,item))))))))))

(defmacro xml-output-hz (hz)
  (let ((item (gensym)))
    `(cl-who:htm
      (:hankaku-zenkaku
       (loop
	  :for ,item
	  :in  ,hz
	  :do  (cl-who:htm (:item :row (row-of ,item)
				  :id    (first (title-of ,item))
				  :title (second (title-of ,item))
				  (:name (cl-who:str (name-of ,item)))
				  (:pre  (cl-who:str (pre-of ,item)))
				  (:post (cl-who:str (post-of ,item))))))))))

(defun repair-xml-output (csv relative must either hanzen)
  (let1 pathname (csv-pathname csv)
    (with-open-file (in (make-pathname :defaults pathname :type "xml")
			:direction :output
			:if-exists :supersede)
      (cl-who:with-html-output (i in :prologue "<?xml version='1.0' encoding='UTF-8'?>" :indent t)
	(:root
	 (:pathname    (cl-who:str (namestring pathname)))
	 (:create-date (cl-who:str (csv-created csv)))
	 (:occur-date  (cl-who:str (csv-occurd csv)))
	 (:hospital
	  (:code (cl-who:str (csv-hcode csv)))
	  (cl-who:str (csv-hname csv)))
	 (xml-output-relative relative)
	 (xml-output-must must)
	 (xml-output-either either)
	 (xml-output-hz hanzen))))))

(defun get-cell-contents (cell)
  (aif (cell-fix-value cell)
       it
       (cell-contents cell)))

(defmethod repair ((l LINE))
  (append (list "5" "2")
	  (mapfn '(line-name line-bango line-birthday line-number) l)
	  (mapcar #'get-cell-contents (line-contents l))))

(defun repair-csv-output-next-pathname (pathname)
  (ppcre:register-groups-bind (date branch)
      (csv-filename-regexp (pathname-name pathname))
    (if (ppcre:scan "9$" branch)
	(error "次の名前を用意できません。")
	(make-pathname :defaults pathname
		       :name (format nil "00263129_FKAC522_~A_~3,'0d"
				     date
				     (1+ (read-from-string branch)))))))

(defun repair-csv-output-make-pathname (pathname)
  (case csv-output-type
    (:exchange pathname)
    (:new      (repair-csv-output-next-pathname pathname))))

(defun repair-csv-output (csv &optional header)
  (let ((body (csv-body csv))
	(head (or header (funcall (csv-index csv) :maine)))
	(file (repair-csv-output-make-pathname (csv-pathname csv))))
    (if (eq csv-output-type :exchange)
	(cl-fad:copy-file (csv-pathname csv)
			  (make-pathname :defaults (csv-pathname csv)
					 :type "csv.bak")
			  :overwrite t))
    (with-open-file (i file :direction :output
    		       :external-format :cp932
		       :if-exists (if (eq csv-output-type :exchange) :supersede :error))
      (format i "1,00263129,京都建築国民健康保険組合,~A,FKAC522~{~A~}~%~{~A~^,~}~%"
    	      (csv-occurd csv)
    	      (make-list (- (length head) 5) :initial-element ",")
    	      head)
      (iter (for line :in body)
    	    (format i "~{~A~^,~}~%" (repair line)))
      (format i "9,~A" (length body)))))

(defmethod repair ((c CSV))
  (multiple-value-bind (r m e h)
      (repaired-multiple-values c)
    (when (or r m e h)
      (repair-xml-output c r m e h)
      (repair-csv-output c))))

;; #P"f:/kcsv2/00263129_FKAC522_20130612_020.csv"
(defun repair2 (csv)
  (multiple-value-bind (r m e h)
      (repaired-multiple-values csv)
    (repair-xml-output csv r m e h)
    (repair-csv-output csv)))

(defun repair-file (path-string)
  (repair (create-csv (make-pathname :defaults path-string))))

(defun directory-list-csv (directory)
  (iter (for file :in-directory directory :type "csv")
	(collect file)))

(defun repair-directory (directory-string)
  (iter (for file :in (directory-list-csv directory-string))
	(if (ppcre:scan csv-filename-regexp (pathname-name file))
	    (time (repair-file file)))))

#|
  一覧を作成する
|#
(defgeneric out (arg hash hash2 op))
(defmethod out ((l LINE) (zhash hash-table) (172hash hash-table) (op stream))
  (declare (ignorable op) (optimize speed))
  (optima:match l
    ((LINE name bango birthday number row shibu merr eerr rerr)
     (format nil "~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
	     row number (kensin:long-shibu shibu)
	     bango
	     name
	     birthday
	     (apply #'+ merr)
	     (apply #'+ eerr)
	     (apply #'+ rerr)
	     ;; (optima:match (the list (gethash number zhash))
	     ;;   ((LIST zenken)
	     ;; 	(optima:match zenken
	     ;; ((ZENKEN::ZENKEN zenken::途中取得日 zenken::途中喪失日)
	     ;; 	   (format nil "~A~A"
	     ;; 		   (aif zenken::途中取得日
	     ;; 			(concatenate 'string it "(取得)") "")
	     ;; 		   (aif zenken::途中喪失日
	     ;; 			(concatenate 'string it "(喪失)") "")))
	     ;;   (nil ""))
	     (optima:match (gethash number zhash)
	       ((ZENKEN::ZENKEN zenken::途中取得日 zenken::途中喪失日)
		(format nil "~A~A"
			(aif zenken::途中取得日
			     (concatenate 'string it "(取得)") "")
			(aif zenken::途中喪失日
			     (concatenate 'string it "(喪失)") "")))
	       (nil ""))
	     (optima:match (gethash number 172hash)
	       ((LIST 172data)
		;; (optima:match 172data
		;;   ((KENSIN::172DATA kensin::健診メッセージ kensin::指導メッセージ)
		;; (if (and (string-null kensin::健診メッセージ)
		;; 	    (string-null kensin::指導メッセージ))
		;;        "○"
		;;        (format nil "~A,~A"
		;; 	       kensin::健診メッセージ kensin::指導メッセージ))))
		(with-slots (kensin::健診メッセージ kensin::指導メッセージ) 172data
		  (if (and (string-null kensin::健診メッセージ)
			   (string-null kensin::指導メッセージ))
		      "○"
		      (format nil "~A,~A"
			      kensin::健診メッセージ kensin::指導メッセージ))))
	       (nil "×"))))))

(defmethod out ((c CSV) (zhash hash-table) (172hash hash-table) (op stream))
  (declare (optimize (speed 3) (safety 0)))
  (optima:match c
    ((CSV body occurd hcode hname pathname)
     (iter (for line :in body)
	   (format op "~A,~A,~A,~A,~A"
		   occurd hcode hname
		   (pathname-name pathname)
		   (out line zhash 172hash op))))))

(defvar *csv-output* "f:/util2/csv/temp.csv")

(defun csvlist (directory-string)
  (with-open-file (o *csv-output*
		     :direction :output
		     :external-format :sjis
		     :if-exists :supersede)
    (format o "~{~A~^,~}~%"
	    '("受診日" "医療機関CD" "医療機関" "ファイル名" "行"
	      "整理番号" "支部" "番号" "氏名" "生年月日"
	      "必須" "選択" "相関" "取得・喪失"))
    (iter (with zhash = (cl-store:restore ksetting::*zenken-hash*))
	  ;; (with zhash = (zenken::make-jusinken-hash #P"f:/20130628/特定健診全件データ.csv"))
      	  (with hash  = (kensin::172-hash))
	  (for file :in (directory-list-csv directory-string))
	  (out (create-csv file) zhash hash o))))

(defparameter xls-width (vector 10 11 15 30 4 12 8.5 6 16 10 5 5 5 5 26))
(defparameter xls-align (vector :right :center :left :center :right :center
			  :center :right :left :right :center :center :center :left :left))

(defun csvlist-xls-width (sheet)
  (iter (with width = xls-width)
	(for col :from 1 :to (length width))
	(excel:set-width sheet
			 (aref util::alph (1- col))
			 (svref width (1- col)))))

(defun csvlist-xls-alignment (sheet)
  (iter (with align = xls-align)
	(for col :from 1 :to (length align))
	(excel:set-alignment sheet
			     (aref util::alph (1- col))
			     :horizontalAlignment
			     (case (svref align (1- col))
			       (:right	excel::xlright)
			       (:center excel::xlcenter)
			       (:left	excel::xlleft)))))

(defun csvlist-xls-head-alignment (sheet)
  (excel:set-alignment sheet 1
		       :horizontalAlignment
		       excel::xlcenter))

(defun csvlist-xls-create (sheet)
  (let ((lr (excel:lastrow sheet :x 1 :y 1)))
    (csvlist-xls-width sheet)
    (csvlist-xls-alignment sheet)
    (csvlist-xls-head-alignment sheet)
    (excel::border sheet (:a 1) (:n lr))))

(defun csvlist-xls-duplicate-check (sheet lastrow)
  (iter (with hash = (make-hash-table :test #'eql))
	(for row :from 2 :to lastrow)
	(for val = (excel::value sheet (:f row)))
	(if (gethash val hash nil)
	    (excel:set-colorindex sheet (:a row) (:n row) :interior excel::xlpink)
	    (progn
	      (setf (gethash val hash) 1)
	      (next-iteration)))))

(defun jnum->nendo-end (jnum)
  (if-match-bind
      ((year (string 2)) _)
      (the simple-string (& write-to-string truncate jnum))
    (nendo-end (+ 2000 (read-from-string year)))))

;; Evaluation took:
;;   24.366 seconds of real time
;;   5.179233 seconds of total run time (3.900025 user, 1.279208 system)
;;   [ Run times consist of 0.093 seconds GC time, and 5.087 seconds non-GC time. ]
;;   21.26% CPU
;;   75,388,767,075 processor cycles
;;   83,579,488 bytes consed

(defun csv-color-classify (v)
  (iter (for line :in v)
	(for row :upfrom 1)
	(optima:match line
	  ((LIST* "受診日" _)
	   (next-iteration))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ _ _ _ sikaku _)
	   (if (and sikaku (string-not-null sikaku))
	       (collect row :into black)
	       (optima:fail)))
	  ((LIST* _ _ _ _ _ jnum _ _ _ birth _)
	   (if (eq 75 (how-old birth (jnum->nendo-end jnum)))
	       (collect row :into black)
	       (optima:fail)))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ _ _ _ _ "○" _)
	   (collect row :into gray))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ _ _ _ _ "×" _)
	   (collect row :into red))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ _ _ _ _ flag _)
	   (if (ppcre:scan "(取得|喪失)" flag)
	       (collect row :into yellow)
	       (optima:fail))))
	(finally (return (values black gray red yellow)))))

(defun color-select (sym)
  (case sym
    (:black	excel::xlgray50)
    (:gray	excel::xlgray25)
    (:white	excel::xlwhite)
    (:red	excel::xlpink)
    (:yellow	excel::xlyellow)))

(defun coloring (sheet nums color)
  (iter (with c = (color-select color))
	(for rows :in nums)
	(optima:match rows
	  ((LIST start end)
	   (set-colorindex sheet (:a start) (:o end) :interior c))
	  ((TYPE ATOM)
	   (set-colorindex sheet (:a rows) (:o rows) :interior c)))))

(defmacro %thread (&rest form)
  `(sb-thread:make-thread
    (lambda () ,@form)))

(defun csvlist-xls (directory-string)
  (declare (optimize (speed 0) (safety 3) (debug 3))
	   (ignorable directory-string))
  (with-excel (app :visible t :quit nil :debugger t)
    (with-excel-book (app book *csv-output* :close nil :debugger t)
      (let* ((sh (ole book :worksheets :item 1))
	     (lr (lastrow sh :x 1 :y 1))
	     (uv (value sh (:a 1) (:o lr))))
	(multiple-value-bind (b g r y) (csv-color-classify uv)
	  (coloring sh b :black)
	  (coloring sh g :gray)
	  (coloring sh r :red)
	  (coloring sh y :yellow))
	(csvlist-xls-create sh)
	(ole sh :range (format nil "A1:O~A" lr) :AutoFilter 1)
	(excel::save-book book "f:/util2/csv/受診者簡易入力シート一覧.xls" :xls)))))

(defmethod line-has-error? ((l line))
  (> (apply #'+ (line-errors l)) 0))

(defun error-by-personal ()
  (iter (with 172hash = (kensin::172-hash))
	(for csv :in-directory topdir :type "csv")
	(for instance = (create-csv csv))
	(appending (iter (for line :in (csv-body instance))
			 (if (line-has-error? line)
			     (collect
				 (append
				  (list (csv-hcode instance)
					(csv-hname instance)
					(csv-occurd instance)
					(pathname-name (csv-pathname instance))
					(line-number line)
					(line-bango line)
					(line-name line))
				  (line-errors line)
				  (aif (gethash (line-number line) 172hash)
				       (list "○" (kensin::172data-資格フラグ (car it))
					     (kensin::172data-健診メッセージ (car it)))
				       (list "" "" "")))))))))

(defun error-by-personal-print (&optional op)
  (format (or op *standard-output*) "~{~A~^,~}~%"
	  (append (list "コード" "機関名" "実施日" "ファイル名" "整理番号"
			"番号" "氏名")
		  (title)))
  (iter (for line :in (error-by-personal))
	(format (or op *standard-output*) "~{~A~^,~}~%" line)))


#|
(defparameter x01d #P"g:/20131016_down/ldl/")
(defparameter x02d #P"g:/20131016_down/hanzen/")
(defparameter x03d #P"g:/20131016_down/hba1c/")

(defun pathname-name2 (pathname)
  (ppcre:register-groups-bind (name ignore)
      ("(00263129_FKAC522_\\d{8}_\\d{3})(.+)" (pathname-name pathname))
    (declare (ignorable ignore))
    name))

(defun files (dir)
  (mapcar (lambda (l)
	    (create-csv
	     (make-pathname :defaults #P"f:/kcsv2/"
			    :name (pathname-name2 l)
			    :type "csv")))
	  (directory-list dir :type "csv")))

(defun repairing-ldl ()
  (iter (for f :in (files x01d))
	(for ind = (csv-index f))
	(iter (for line :in (csv-body f))
	      (iter (for cell :in (line-contents line))
				      (if (equal "3F077000002327101" (car (funcall ind :num2id (cell-x cell))))
					  (with-slots (contents) cell
					    (setf contents (format nil "~A" (round (read-from-string contents))))))))
	(print (csv-pathname f))
	(repair2 f)))

(defun replace-if (list predicate thunk)
  (labels ((in (subl r)
	     (if (null subl)
		 (reverse r)
		 (in (cdr subl)
		     (cons (if (funcall predicate (car subl))
			       (funcall thunk (car subl))
			       (car subl))
			   r)))))
    (in list '())))

(defun hba1c-ngsp-title (csv)
  (replace-if (funcall (csv-index csv) :maine)
	      (lambda (car) (ppcre:scan "3D045" car))
	      (lambda (car)
		(ppcre:register-groups-bind (other)
		    ("3D045(.+)" car)
		  (format nil "3D046~A" other)))))

(defun hba1c-ngsp-line-repair (index line)
  (iter (for cell :in (line-contents line))
	(for title = (car (funcall index :num2id (cell-x cell))))
	(if (ppcre:scan "3D046" title)
	    (with-slots (contents) cell
	      (setf contents
		    (if (string-null contents)
			""
			(funcall (compose #'write-to-string
					  (lambda (l)
					    (float (/ (truncate (+ 0.4 l) 0.1) 10)))
					  #'read-from-string)
				 contents)))))))

(defun hba1c-files ()
  (iter (for f :in (files x03d))
	(funcall (csv-index f) :maine! (hba1c-ngsp-title f))
	(iter (for line :in (csv-body f))
	      (hba1c-ngsp-line-repair (csv-index f) line))
	(repair2 f)))

(defun hz-files ()
  (iter (for f :in (files x02d))
	(repair2 f)))

(defparameter x01f #P"f:/kcsv2/00263129_FKAC522_20130612_020.csv")
(defparameter x02f #P"f:/kcsv2/00263129_FKAC522_20130926_120.csv")

|#

(in-package :cl-user)
