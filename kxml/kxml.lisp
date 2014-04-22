(in-package :kxml)

(defparameter *level* 1)

(defvar minimum-code-base
  '(("9N001000000000001" . "身長")
    ("9N006000000000001" . "体重")
    ("9N011000000000001" . "ＢＭＩ")
    ("9N016160100000001" . "腹囲(実測）")
    ("9N056000000000011" . "既往歴")
    ("9N056160400000049" . "（具体的な既往歴）")
    ("9N061000000000011" . "自覚症状")
    ("9N061160800000049" . "（所見）")
    ("9N066000000000011" . "他覚症状")
    ("9N066160800000049" . "（所見）")
    (("9A755000000000001" . "収縮期血圧（その他）")
     ("9A751000000000001" . "収縮期血圧（１回目）")
     ("9A752000000000001" . "収縮期血圧（２回目）"))
    (("9A765000000000001" . "拡張期血圧（その他）")
     ("9A762000000000001" . "拡張期血圧（２回目）")
     ("9A761000000000001" . "拡張期血圧（１回目）"))
    ("9N141000000000011" . "採血時間（食後）")
    (("3F015000002327101" . "中性脂肪（トリグリセリド）")
     ("3F015000002327201" . "中性脂肪（トリグリセリド）")
     ("3F015000002399901" . "中性脂肪（トリグリセリド）"))
    (("3F070000002327101" . "ＨＤＬコレステロール")
     ("3F070000002327201" . "ＨＤＬコレステロール")
     ("3F070000002399901" . "ＨＤＬコレステロール"))
    (("3F077000002327101" . "ＬＤＬコレステロール")
     ("3F077000002327201" . "ＬＤＬコレステロール")
     ("3F077000002399901" . "ＬＤＬコレステロール"))
    (("3B035000002327201" . "GOT（ＡＳＴ）")
     ("3B035000002399901" . "GOT（ＡＳＴ）"))
    (("3B045000002327201" . "GPT（ＡＬＴ）")
     ("3B045000002399901" . "GPT（ＡＬＴ）"))
    (("3B090000002327101" . "γ-GT(γ-GTP)")
     ("3B090000002399901" . "γ-GT(γ-GTP)"))
    (("3D010000001926101" . "空腹時血糖")
     ("3D010000002227101" . "空腹時血糖")
     ("3D010000001927201" . "空腹時血糖")
     ("3D010000001999901" . "空腹時血糖"))
    (("3D045000001906202" . "ＨｂＡ１ｃ")
     ("3D045000001920402" . "ＨｂＡ１ｃ")
     ("3D045000001927102" . "ＨｂＡ１ｃ")
     ("3D045000001999902" . "ＨｂＡ１ｃ"))
    (("1A020000000191111" . "尿糖")
     ("1A020000000190111" . "尿糖"))
    (("1A010000000191111" . "尿蛋白")
     ("1A010000000190111" . "尿蛋白"))
    ("9N501000000000011" . "メタボリックシンドローム判定")
    ("9N506000000000011" . "保健指導レベル")
    ("9N511000000000049" . "医師の診断（判定）")
    ("9N516000000000049" . "健康診断を実施した医師の氏名")
    ("9N701000000000011" . "服薬１（血圧）")
    ("9N706000000000011" . "服薬２（血糖）")
    ("9N711000000000011" . "服薬３（脂質）")
    ("9N716000000000011" . "既往歴１（脳血管）")
    ("9N721000000000011" . "既往歴２（心血管）")
    ("9N726000000000011" . "既往歴３（腎不全・人工透析）")
    ("9N731000000000011" . "貧血")
    ("9N736000000000011" . "喫煙")))

(defmacro with-kxml-namespace (&rest body)
  `(xpath:with-namespaces (("u" "urn:hl7-org:v3"))
     ,@body))

(defun pairp (obj)
  (and (listp obj) (cdr obj) (not (listp (cdr obj)))))

(defun minimum-code ()
  (iter (for pair :in minimum-code-base)
	(collect (if (pairp pair)
		     (car pair)
		     (mapcar #'car pair)))))

;; ------------------------------------------------------------

(defun %create-xpath-element (sym &key (top t))
  (cl-irregsexp:if-match-bind
   ("<" (key (+ (or (- #\0 #\9) (- #\a #\z) (- #\A #\Z)))) ">" (last))
   (the string sym)
   (the string (format nil "/u:~A" (gethash key *keyhash*)))
   (the string (format nil "/u:~A" sym))))

(defun %create-xpath (string-list &key (top t))
  (iter (for sym :in string-list)
	(collect (optima:match sym
		   ((LIST x y)   (format nil "[@~A='~A']" x y))
		   ((LIST x)     (format nil "/@~A" x))
		   ((type ATOM)  (%create-xpath-element sym :top top)))
	  :into pot)
	(finally (return (format nil "~{~A~}" pot)))))

(defparameter baselist
  `(("cd" . "ClinicalDocument")
    ("rt" . "ClinicalDocument/u:recordTarget")
    ("pr" . "ClinicalDocument/u:recordTarget/u:patientRole")
    ("aa" . ,(subseq
	      (%create-xpath '("ClinicalDocument" "author" "assignedAuthor" "representedOrganization"))
	      3))
    ("cs" . ,(subseq
	      (%create-xpath '("ClinicalDocument" "component" "structuredBody" "component" "section"))
	      3))))

(defparameter *keyhash*
  (iter (for l :in baselist)
	(shash l :condition t
	       :key #'car
	       :value #'cdr)))
;; ------------------------------------------------------------
(defun %purify (string)
  (coerce
   (remove-if (lambda (ch) (char-equal ch #\zero_width_no-break_space))
	      (coerce string 'list))
   'string))

(defun string-to-stp (string)
  (cxml:parse string (stp:make-builder)))

(defun %transform (entry)
  (string-to-stp
   (%purify
    (sb-ext:octets-to-string
     (zip:zipfile-entry-contents entry)
     :external-format :UTF-8))))

(defmacro xpath (func docs &rest xpath-string)
  `(with-kxml-namespace
       (funcall ,func
		(xpath:evaluate
		 (%create-xpath ',xpath-string)
		 ,docs))))

;; ------------------------------------------------------------
(xrep::defclass% XML-PARSER ()
  (pathname	path-of)
  (stream	stream-of)
  (entry	entry-of)
  (documents	docs-of)
  (zipfile	zipfile-of))

(defgeneric insurance-number		(obj))
(defgeneric knumber			(obj))
(defgeneric bnumber			(obj))
(defgeneric jnumber			(obj))
(defgeneric postal-code			(obj))
(defgeneric address			(obj))
(defgeneric name			(obj))
(defgeneric gender			(obj))
(defgeneric birthday			(obj))
(defgeneric create-day			(obj))
(defgeneric occur-day			(obj))
(defgeneric hospital-code		(obj))
(defgeneric hospital-name		(obj))
(defgeneric hospital-postal-code	(obj))
(defgeneric hospital-address		(obj))
(defgeneric doctor			(obj))
(defgeneric doctor-clojure		(obj))
(defgeneric level1			(obj))
(defgeneric level1-hash			(obj))
(defgeneric level1-base			(obj))
(defgeneric level2			(obj))
(defgeneric minimum-results		(obj))
(defgeneric minimum%			(obj))
(defgeneric minimum			(obj &rest key))
(defgeneric xmlname			(obj))
(defgeneric zip-basename		(obj))
(defgeneric info			(obj))
(defgeneric path-basename		(obj))
(defgeneric write-out			(obj &optional pathname))

;; ------------------------------------------------------------
(defun %get-address (xpath-node)
  (xpath:string-value
   (stp:find-child-if
    (lambda (child) (and (typep child 'stp:text)
			 (ppcre:scan "[都道府県市町村]"
				     (xpath:string-value child))))
    (car (xpath::pipe-of xpath-node)))))

(defun %get-gender (xpath-node)
  (string-case (xpath:string-value xpath-node)
    ("1" "男")
    ("2" "女")
    (t "不明")))

(defun date-normalize (node)
  (normal-date->string
   (strdt (xpath:string-value node))))

(defun time-width (node)
  (let* ((stp (car (xpath::pipe-of node)))
	 (v (find-attribute stp "value"))
	 (u (find-attribute stp "unit")))
    (format nil "~A~A"
	    (xpath:string-value v)
	    (xpath:string-value u))))

(defun support-level (string-number)
  (string-case string-number
    ("1" "積極的支援")
    ("2" "動機付支援")))

(defmacro xpath-generator (docs &rest args)
  `(xpath ,(case (car args)
		 (:string   '#'xpath:string-value)
		 (:slevel   '#'support-level)
		 (:address  '#'%get-address)
		 (:gender   '#'%get-gender)
		 (:date     '#'date-normalize)
		 (:width    '#'time-width)
		 (:identity '#'identity))
	  ,docs
	  ,@(cdr args)))

(defmacro def-xml-parser-method (name &rest args)
  `(defmethod ,name ((x XML-PARSER))
     (xpath-generator (docs-of x)
		      ,@args)))

(defun child-elements (element)
  (stp:filter-children (lambda (o) (typep o 'stp:element))
		       element))

(defun elements-classify (element)
  (let1 child (child-elements element)
    (values (find-if (lambda (o) (equal "code" (stp:local-name o))) child)
	    (find-if (lambda (o) (equal "value" (stp:local-name o)))
		     child))))

(defun find-attribute (element name)
  (if element
      (stp:find-attribute-named element name)
      nil))

(defun string-value (node)
  (if node
      (xpath:string-value node)
      nil))

;; ------------------------------------------------------------
(def-clojure results (c v)
  ((code  (string-value (find-attribute c "code")))
   (value (string-value (or (find-attribute v "value")
			    (find-attribute v "code")
			    (find-attribute v "type")
			    v)))
   (name  (string-value (find-attribute c "displayName"))))
  (:code!  (c) (setf code c))
  (:value! (c) (setf value c))
  (:name!  (c) (setf name c)))

;; ------------------------------------------------------------
(def-xml-parser-method insurance-number
    :string "<pr>" "id[@root='1.2.392.200119.6.101']" ("extension"))
(def-xml-parser-method knumber
    :string "<pr>" "id[@root='1.2.392.200119.6.204']" ("extension"))
(def-xml-parser-method bnumber
    :string "<pr>" "id[@root='1.2.392.200119.6.205']" ("extension"))
(def-xml-parser-method jnumber
    :string "<cd>" "participant" "associatedEntity" "id" ("extension"))
(def-xml-parser-method postal-code
    :string "<pr>" "addr" "postalCode")
(def-xml-parser-method address
    :address "<pr>" "addr")
(def-xml-parser-method name
    :string "<pr>" "patient" "name")
(def-xml-parser-method gender
    :gender "<pr>" "patient" "administrativeGenderCode" ("code"))
(def-xml-parser-method birthday
    :date "<pr>" "patient" "birthTime" ("value"))
(def-xml-parser-method create-day
    :date "<cd>" "author" "time" ("value"))
(def-xml-parser-method occur-day
    :date "<cd>" "documentationOf" "serviceEvent" "effectiveTime" ("value"))
(def-xml-parser-method hospital-code
    :string "<aa>" "id" ("extension"))
(def-xml-parser-method hospital-name
    :string "<aa>" "name")
(def-xml-parser-method hospital-postal-code
    :string "<aa>" "addr" "postalCode")
(def-xml-parser-method hospital-address
    :address "<aa>" "addr")
(def-xml-parser-method doctor
    :string "<cd>" "component" "structuredBody" "component" "section" "entry"
    "observation" "author" "assignedAuthor" "assignedPerson" "name")

(defun stp-child-element (stp)
  (stp:filter-children (lambda (o) (typep o 'stp:element))
		       stp))

(defun stp-to-result-clojure (stp)
  (let1 children (stp-child-element stp)
    (results (find-if (lambda (o) (equal "code" (stp:local-name o))) children)
	     (find-if (lambda (o) (equal "value" (stp:local-name o))) children))))

(defmethod level1-base ((x XML-PARSER))
  (xpath:map-node-set->list
   #'stp-to-result-clojure
   (xpath #'identity (docs-of x)
	  "ClinicalDocument" "component" "structuredBody"
	  "component" "section" "entry" "observation")))

(defmethod doctor-clojure ((x XML-PARSER))
  (let1 cl (results nil nil)
    #[cl :code!  "9N516000000000049"]
    #[cl :value! (doctor x)]
    #[cl :name!  "健康診断を実施した医師の氏名"]
    cl))

(defmethod level1 ((x XML-PARSER))
  (cons (doctor-clojure x)
	(remove-if-not #[:value]
		       (level1-base x))))

(defmethod level1-hash ((x XML-PARSER))
  (iter (for clojure :in (level1 x))
	(shash clojure
	       :condition t
	       :key   #[:code]
	       :value #[:value])))

(defmethod level2 ((x XML-PARSER))
  (xpath:map-node-set->list
   #'stp-to-result-clojure
   (xpath #'identity (docs-of x)
	  "ClinicalDocument" "component" "structuredBody"
	  "component" "section" "entry" "observation" "entryRelationship" "observation")))

(defmethod minimum-results ((x XML-PARSER))
  (iter (with hash = (level1-hash x))
	(for codes :in (minimum-code))
	(collect (if (listp codes)
		     (iter (for c :in codes)
			   (aif (gethash c hash)
				(leave it)))
		     (gethash codes hash "")))))

(defmethod %minimum ((x XML-PARSER))
  (append (mapcar (lambda (f) #[f x])
		  (list #'jnumber #'knumber #'bnumber #'name #'birthday
			#'occur-day #'hospital-code #'hospital-name))
	  (minimum-results x)))

(defmethod minimum ((x XML-PARSER) &key (string nil))
  (funcall
   (if string (lambda (l) (format nil "~{~A~^,~}~%" l))
       #'identity)
   (%minimum x)))

(defmethod xmlname ((x XML-PARSER))
  (last1 (ppcre:split "/" (path-of x))))

(defmethod zip-basename ((x XML-PARSER))
  (pathname-name (make-pathname :defaults (zipfile-of x))))

(defmethod info ((x XML-PARSER))
  (mapcar
   (lambda (f) #[f x])
   (list #'zip-basename #'xmlname #'jnumber #'knumber #'bnumber
	 #'name #'birthday #'hospital-code #'hospital-name #'create-day #'occur-day)))

(defmethod path-basename ((x XML-PARSER))
  (last1 (ppcre:split "/" (path-of x))))

(defmethod write-out ((x XML-PARSER) &optional pathname)
  (call-with-output-file2
      (or pathname (path-basename x))
    (lambda (op)
      (format op "~A"
	      (xrep::stp-to-string
	       (stp:insert-child (docs-of x)
				 (stp:make-processing-instruction "xml-stylesheet"
								  "href='file:///f:/util2/kxml/hsido.xsl' type='text/xsl'")
				 0))))))

;; ------------------------------------------------------------
(defmethod initialize-instance :after ((x XML-PARSER) &rest args)
  (declare (ignorable args))
  (with-slots (pathname stream entry documents) x
    (setq stream	(if entry (zip::zipfile-entry-stream entry))
	  documents	(if entry
			    (%transform entry)
			    (cxml:parse-file pathname (stp:make-builder))))))

;; ------------------------------------------------------------
(defun %map-with-kxml (func zipfile)
  (let1 n nil
    (zip:with-zipfile (z zipfile)
      (zip:do-zipfile-entries (key entry z)
	(if (ppcre:scan "DATA/[hg]\\d{26}" key)
	    (push (funcall func key entry zipfile) n))))
    n))

(defun map-with-kxml (func zipfile)
  (%map-with-kxml
   (lambda (k e z)
     (let1 obj (make-instance 'XML-PARSER :pathname k :entry e :zipfile z)
       (funcall func obj)))
   zipfile))

(defun csvname (zipfile)
  (make-pathname :defaults zipfile
		 ;; :directory nil
		 :type "csv"))

(defun make-title (func op)
  (flet ((f (arg) (format op "~{~A~^,~}~%" arg)))
    (let ((list (append (make-list 8 :initial-element "")
			(mapcar
			 (lambda (l)
			   (funcall func (if (listp l) (car l) l)))
			 (minimum-code)))))
      (if (eq op :list)
	  list
	  (f list)))))

(defun english-title (op)  (make-title #'identity op))
(defun japanese-title (op) (make-title #'kcsv::code->title op))

(defun parse-zipfile (zipfile)
  (map-with-kxml #'identity zipfile))

(defun kxml-to-csv (zipfile)
  (call-with-output-file2 (csvname zipfile)
    ;; (lambda (op)
    ;;   (english-title op)
    ;;   (japanese-title op)
    ;;   (format op "~{~A~}"
    ;; 	      (map-with-kxml (lambda (obj) (minimum obj :string t))
    ;; 			     zipfile)))
    (lambda (op)
      (let* ((list `(,(english-title :list)
		     ,(japanese-title :list)
		     ,@(map-with-kxml #'minimum zipfile)))
	     (vertical-list
	      (reduce (lambda (x y) (mapcar #'cons y x))
		      (cdr list)
		      :initial-value (mapcar #'list (car list)))))
	(format op "~{~{~A~^,~}~%~}" (mapcar #'reverse vertical-list))))
    :code :SJIS))

;; f:/zip/MAIN/2013/2612800710_00263129_201312280_1.zip

(defun zip-info (zipfile)
  (map-with-kxml #'info zipfile))

(in-package :cl-user)
