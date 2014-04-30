(in-package :hx)

(defgeneric common-section		(x))
(defgeneric supported-level		(x))
(defgeneric changing-stage		(x))
(defgeneric course-name		(x))
(defgeneric first-time-performer	(x))
(defgeneric first-time-date		(x))
(defgeneric first-time-style	(x))
(defgeneric first-time-width	(x))
(defgeneric target			(x))
(defgeneric target-base		(x))
(defgeneric final			(x))
(defgeneric final-base		(x))
(defgeneric first-time-style	(x))
(defgeneric support-base		(x))
(defgeneric support			(x))
(defgeneric internal-performer	(x))
(defgeneric internal-date		(x))
(defgeneric internal-style		(x))
(defgeneric internal-width		(x))
(defgeneric internal-base		(x))
(defgeneric internal			(x))
(defgeneric final-assess		(x))
(defgeneric xml-stored-p		(x))
(defgeneric index-string		(x op))
(defgeneric xls-string		(x op))

(kxml::def-xml-parser-method supported-level
    :slevel "<cs>" "code[@code='90010']/.." "entry" "act" "entryRelationship"
    "observation" "code[@code='1020000001']/.." "value" ("code"))

(kxml::def-xml-parser-method changing-stage
    :string "<cs>" "code[@code='90010']/.." "entry" "act" "entryRelationship"
    "observation" "code[@code='1020000002']/.." "value" ("code"))

(kxml::def-xml-parser-method course-name
    :string "<cs>" "code[@code='90010']/.." "entry" "act" "entryRelationship"
    "observation" "code[@code='1020000003']/.." "value")

(kxml::def-xml-parser-method first-time-performer
    :string "<cs>" "code[@code='90030']/.." "entry" "act" "performer"
    "assignedEntity" "code" ("code"))

(kxml::def-xml-parser-method first-time-date
    :date "<cs>" "code[@code='90030']/.." "entry" "act" "effectiveTime" ("value"))

(kxml::def-xml-parser-method first-time-style
    :string "<cs>" "code[@code='90030']/.." "entry" "act" "code" ("code"))

(kxml::def-xml-parser-method first-time-width
    :width "<cs>" "code[@code='90030']/.." "entry" "act" "entryRelationship"
    "observation" "code[@code='1022000013']/.." "effectiveTime" "width")

(kxml::def-xml-parser-method target-base
    :identity "<cs>" "code[@code='90030']/.." "entry" "act" "entryRelationship"
    "observation[@moodCode='GOL']")

(defun find-child (element name)
  (stp:find-child-if
   (lambda (ch)
     (and (typep ch 'stp:element)
	  (equal name (stp:local-name ch))))
   element))

(defun find-attribute (element name)
  (xpath:string-value
   (stp:find-attribute-named element name)))

(defun base-to-clojure-list (list)
  (xpath:map-node-set->list
   (lambda (element)
     (let* ((code  (find-child element "code"))
	    (value (find-child element "value"))
	    (cloj  (kxml::results code value)))
       #[cloj :name! (gethash #[cloj :code] hsido-id)]
       cloj))
   list))

(defmethod target ((x XML-PARSER))
  (base-to-clojure-list (target-base x)))

(kxml::def-xml-parser-method final-base
    :identity "<cs>" "code[@code='90060']/.." "entry" "act" "entryRelationship" "observation")

(defmethod final ((x XML-PARSER))
  (base-to-clojure-list (final-base x)))

(kxml::def-xml-parser-method support-base
    :identity "<cs>" "code[@code='90040']/.." "entry" "act")

(defun support-act-list (stp)
  (xpath:map-node-set->list
   #'identity
   (xpath #'identity (stp:root stp)
	  "<cs>" "code[@code='90040']/.." "entry" "act" "entryRelationship" "observation")))

(defmacro string+ (&rest args)
  (let ((i (gensym)))
    `(with-output-to-string (,i)
       (iter (for str :in ',args)
	     (format ,i "~A" str)))))

(defun support-width (doc code)
  (kxml::with-kxml-namespace
      (let ((xpath:*navigator* (cxml-xmls:make-xpath-navigator)))
	(xpath:string-value
	 (xpath:evaluate 
	  (format nil "//u:code[@code='~A']/..~A~A"
		  code "/u:effectiveTime" "/u:width/@value")
	  doc)))))

(defun support-point (doc code)
  (kxml::with-kxml-namespace
      (let ((xpath:*navigator* (cxml-xmls:make-xpath-navigator)))
	(xpath:string-value
	 (xpath:evaluate 
	  (format nil "//u:code[@code='~A']/../u:value/@value" code )
	  doc)))))

(defmacro string-null-or (&rest args)
  (if args
      `(if (string-not-null ,(car args))
	   ,(car args)
	   (string-null-or ,@(cdr args)))
      nil))

(def-clojure support-clojure (stp)
  ((stp		stp)
   (code	(find-child stp "code"))
   (date	(find-child stp "effectiveTime"))
   (performer	(stp:find-recursively-if
		 (lambda (node) (and (typep node 'stp:element)
				     (equal (stp:local-name node) "code")))
		 (find-child stp "performer")))
   (value	(support-act-list stp))
   (xmls	(stp:serialize stp (cxml-xmls:make-xmls-builder)))
   (type        (if (string-null (support-width xmls "1032200013"))
   		    :typeA :typeB)))
  (:bcode      () (find-attribute code "code"))
  (:bdate      () (kxml::date-normalize
		   (find-attribute date "value")))
  (:bperformer () (find-attribute performer "code"))
  (:bwidth     () (string-null-or
		   (support-width xmls "1032200013")
		   (support-width xmls "1032100013")))
  (:bpoint     () (string-null-or
		   (support-point xmls "1032200014")
		   (support-point xmls "1032100014"))))

(defmethod support ((x XML-PARSER))
  (xpath:map-node-set->list
   #'support-clojure
   (support-base x)))

(defmacro def-xml-parser-intern-method (name symbol &rest args)
  `(kxml::def-xml-parser-method ,name
       ,symbol "<cs>" "code[@code='90050']/.." "entry" "act"
       ,@args))

(def-xml-parser-intern-method internal-performer
    :string "performer" "assignedEntity" "code" ("code"))

(def-xml-parser-intern-method internal-date
    :date "effectiveTime" ("value"))

(def-xml-parser-intern-method internal-style
    :string "code" ("code"))

(def-xml-parser-intern-method internal-width
    :string "entryRelationship" "observation" "code[@code='1032000013']/.."
    "effectiveTime" "width" ("value"))

(def-xml-parser-intern-method internal-base
    :identity "entryRelationship" "observation")

(defmethod internal ((x XML-PARSER))
  (base-to-clojure-list (internal-base x)))

(defmethod final-assess ((x XML-PARSER)))

(defvar xml-directory #P"f:/zip/MAIN/HSIDO/xml/")

(defmethod xml-stored-p ((x XML-PARSER))
  (cl-fad:file-exists-p
   (merge-pathnames xml-directory
		    (kxml::path-basename x))))

(defmethod xml-store ((x XML-PARSER))
  (unless (xml-stored-p x)
    (kxml::write-out x
		     (merge-pathnames xml-directory
				      (kxml::path-basename x)))))

(defun occurd->nendo (parser)
  (& nendo-year strdt kxml::occur-day parser))

(defun jnumber->nendo (parser)
  (handler-case
      (+ 2000
	 (read-from-string
	  (string-take
	   (format nil "~11,'0d"
		   (read-from-string (kxml::jnumber parser)))
	   2)))
    ;; 受診券番号が入っていない場合
    (sb-kernel::bounding-indices-bad-error (e)
      (declare (ignorable e)) nil)
    ;; 受診券番号が入っていない場合
    (end-of-file (e)
      (declare (ignorable e)) nil)))

(defun make-xml-list ()
  (iter (for file :in-allf xml-directory :type "xml")
	(for instance = (make-instance 'kxml::XML-PARSER
				       :entry nil
				       :pathname file))
	(phash instance :condition t
	       :key (lambda (o) (or (jnumber->nendo o)
				    (occurd->nendo o))))))

(defmethod index-string ((x XML-PARSER) op)
  (declare (ignorable op))
  (format nil "~A ~A-~A ~A ~A ~A ~A"
	  (kxml::jnumber x)
	  (kxml::knumber x)
	  (kxml::bnumber x)
	  (kxml::name x)
	  (kxml::occur-day x)
	  (kxml::hospital-code x)
	  (kensin::code->hospital
	   (kxml::hospital-code x))))

(defun make-xml-index-html (&optional op)
  (let1 o (or op *standard-output*)
    (cl-who:with-html-output (o)
      (:html :lang "ja"
	     (:head
	      (:meta :http-equiv "Content-Type" :content "text/html"
		     :charset "UTF-8"))
	     (:body
	      (:script :language "JavaScript1.2" :src "tablesort.js")
	      (loop
		 for (year . data) in (sort (hash-table-alist (make-xml-list))
					    (lambda (x y) (> (car x) (car y))))
		 do (cl-who:htm (:h3 (cl-who:str year))
				(:table :border "1" :cellspacing "0"
					(:thead
					 (:tr
					  (:th (cl-who:str "ファイル名"))
					  (:th :class "case" :width "120" (cl-who:str "受診券整理番号"))
					  (:th :width "120" (cl-who:str "記号番号"))
					  (:th :class "case" :width "120" (cl-who:str "氏名"))
					  (:th :class "case" :width "120" (cl-who:str "受診日"))
					  (:th :class "case" :width "120" (cl-who:str "医療機関CD"))
					  (:th :width "120" (cl-who:str "医療機関"))))
					(loop
					   for xp in (sort (copy-list data)
							   (lambda (x y) (string< (kxml::name x) (kxml::name y))))
					   do (cl-who:htm
					       (:tr :bgcolor (if (final xp) "greenyellow" "white")
						    (:td (:a :href (format nil "file:///~A" (kxml::path-of xp))
							     (cl-who:str (pathname-name
									  (kxml::path-of xp)))))
						    (:td (cl-who:str (kxml::jnumber xp)))
						    (:td (cl-who:str (format nil "~A-~A"(kxml::knumber xp) (kxml::bnumber xp))))
						    (:td (cl-who:str (kxml::name xp)))
						    (:td (cl-who:str (kxml::occur-day xp)))
						    (:td (cl-who:str (kxml::hospital-code xp)))
						    (:td (cl-who:str (kensin::code->hospital (kxml::hospital-code xp)))))))))))))))

(defun make-xml-index ()
  (call-with-output-file2
      (merge-pathnames xml-directory "index.html")
    #'make-xml-index-html))

(defun make-all-xml ()
  (for-each
   (lambda (z) (kxml::map-with-kxml #'xml-store z))
   (allf #P"f:/zip/MAIN/HSIDO/" :type "zip")))

(in-package :cl-user)

