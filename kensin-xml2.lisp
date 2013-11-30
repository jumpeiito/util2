(in-package :kensin)

;; ClinicalDocument
;;   effectiveTime		作成日
;;   recordTarget
;;     patientRole
;;       id root="1.2.392.200119.6.101"	保険者番号
;;       id root="1.2.392.200119.6.204"	被保証記号
;;       id root="1.2.392.200119.6.205"	被保証番号
;;       patient
;;         name				名前
;;         administrativeGenderCode	性別
;;         birthTime			生年月日
;;   author
;;     assignedAuthor
;;       representedOrganization
;;         id root="1.2.392.200119.6.102"	医療機関コード
;;         name				医療機関名
;;   participant
;;     associatedEntity
;;       id root="1.2.392.200119.6.209.100263129" 受診券整理番号
;;   documentationOf
;;     serviceEvent
;;       effectiveTime			実施日
;;   component
;;     structuredBody
;;       component
;;         section
;;           code code="90010"		指導共通情報セクション
;; 	  entry
;;             act
;;               entryRelationship
;;                 code="1020000001"	支援レベル
;; 		code="1020000002"	行動変容ステージ
;; 		code="1020000003"	保健指導コース名
;;       component
;;         section
;;           code code="90060"		最終評価情報セクション
;;           entry
;;             act
;;               effectiveTime		最終評価実施日
;;               performer
;;                 assignedEntity
;;                   code			6か月後の実施者

;; xml-parse
(defun create-xpath (string-list)
  (labels ((in (subl r)
	     (if (null subl)
		 r
		 (in (cdr subl)
		     (str+ r
			   (if (listp (car subl))
			       (if (singlep (car subl))
				   (format nil "/@~A"  (caar subl))
				   (format nil "[@~A='~A']"  (caar subl) (cadar subl)))
			       (format nil "/u:~A" (car subl))))))))
    (the string (in string-list '()))))

(defmacro xpath (func entry-or-dom &rest xpath-string)
  `(xpath:with-namespaces (("u" "urn:hl7-org:v3"))
     (funcall ,func
	      (xpath:evaluate
	       (create-xpath ',xpath-string)
	       ,(if (typep entry-or-dom 'rune-dom::element)
		    entry-or-dom
		    `(entry->dom ,entry-or-dom))))))

(defmacro node-set->dom ()
  (let ((obj (gensym)))
    `(lambda (,obj) (car (xpath:map-node-set->list #'identity ,obj)))))

(defmacro node-set->value ()
  `(lambda (o) (xpath:map-node-set->list
		(lambda (i) (dom:node-value i)) o)))

;; (defun get-elements-function (text)
;;   (if-multiple-bind (ig name) (ppcre:scan "/" text)
;;     #'xpath:string))

(defmacro if-multiple-bind (val form then-form else-form)
  `(if ,form
       (multiple-value-bind ,val ,form ,@then-form)
      ,@else-form))

(defmacro get-elements (doc &rest name)
  (if (cdr name)
      (if (listp (last1 name))
	  (if (ppcre:scan "/" (car (last1 name)))
	      (rxmatch-bind "(.+)/(.+)" (car (last1 name)) (ig1 text)
		(declare (ignorable ig1))
		`(xpath:string-value (dom:get-attribute-node (get-elements ,doc ,@(butlast name)) ,text)))
	      `(dom:get-attribute-node (get-elements ,doc ,@(butlast name)) ,(car (last1 name))))
	  (if (ppcre:scan "/" (last1 name))
	      (rxmatch-bind "(.+)/(.+)" (last1 name) (ig1 text)
		(declare (ignorable ig1))
		`(xpath:string-value (aref (dom:get-elements-by-tag-name (get-elements ,doc ,@(butlast name)) ,text) 0)))
	      `(aref (dom:get-elements-by-tag-name (get-elements ,doc ,@(butlast name)) ,(last1 name)) 0)))
      (if (listp (last1 name))
	  (if (ppcre:scan "/" (car (last1 name)))
	      (rxmatch-bind "(.+)/(.+)" (car (last1 name)) (ig1 text); (ppcre:scan "/" (car (last1 name)))
		(declare (ignorable ig1))
		`(xpath:string-value (dom:get-attribute-node ,doc ,text)))
	      `(dom:get-attribute-node ,doc ,(car (last1 name))))
	  (if (ppcre:scan "/" (last1 name))
	      (rxmatch-bind "(.+)/(.+)" (last1 name) (ig1 text); (ppcre:scan "/" (last1 name))
		(declare (ignorable ig1))
		`(xpath:string-value (aref (dom:get-elements-by-tag-name ,doc ,text) 0)))
	      `(aref (dom:get-elements-by-tag-name ,doc ,(last1 name)) 0)))))

;; (get-elements 'doc "hoge" "foo" ("text/buz"))
;; (get-elements 'doc "hoge")

(defun dom-parse-test (dom)
  (xpath:with-namespaces (("u" "urn:hl7-org:v3"))
    (xpath:evaluate
     ;; (create-xpath '("/patientRole"))
     "//u:patientRole"
     dom)))

(defun create-date (entry)
  (xpath #'xpath:string-value entry
	 "ClinicalDocument" "effectiveTime" ("value")))

(defun patientRole (entry)
  (xpath (node-set->dom)
	 entry
	 "ClinicalDocument" "recordTarget" "patientRole"))

(defun patient (entry)
  (let1 p (get-elements (patientRole entry) "patient")
    (list (get-elements p "text/name")
	  (get-elements p "administrativeGenderCode" ("text/code"))
	  (get-elements p "birthTime" ("text/value")))))

(defun patient-id (entry)
  (macrolet ((value (str) `(dom:value (dom:get-attribute-node v ,str))))
    (iter (with ary = (make-array 3))
	  (for v :in-vector (dom:get-elements-by-tag-name (patientRole entry) "id"))
	  (cond
	    ((equal (value "root") "1.2.392.200119.6.101")
	     (setf (aref ary 0) (value "extension")))
	    ((equal (value "root") "1.2.392.200119.6.204")
	     (setf (aref ary 1) (value "extension")))
	    ((equal (value "root") "1.2.392.200119.6.205")
	     (setf (aref ary 2) (value "extension"))))
	  (finally (return (coerce ary 'list))))))

(defun representedOrganization (entry)
  (xpath (node-set->dom)
	 entry
	 "ClinicalDocument" "author" "assignedAuthor" "representedOrganization"))

(defun hospital (entry)
  (let1 p (representedOrganization entry)
    (list (get-elements p "id" ("text/extension"))
	  (get-elements p "text/name"))))

(defun jnumber (entry)
  (xpath #'xpath:string-value entry
	 "ClinicalDocument" "participant" "associatedEntity" "id" ("extension")))

(defun occur-day (entry)
  (xpath #'xpath:string-value entry
	 "ClinicalDocument" "documentationOf" "serviceEvent" "effectiveTime" ("value")))

(defun common-level (entry)
  (xpath #'xpath:string-value
	 entry
	 "ClinicalDocument" "component" "structuredBody" "component" "section/u:code[@code='90010']/../"
	 "entry" "act" "entryRelationship" "observation" "code[@code='1020000001']/../"
	 "value" ("code")))

(defun common-stage (entry)
  (xpath #'xpath:string-value
	 entry
	 "ClinicalDocument" "component" "structuredBody" "component" "section/u:code[@code='90010']/../"
	 "entry" "act" "entryRelationship" "observation" "code[@code='1020000002']/../"
	 "value" ("code")))

(defun common-course-name (entry)
  (xpath #'xpath:string-value
	 entry
	 "ClinicalDocument" "component" "structuredBody" "component" "section/u:code[@code='90010']/../"
	 "entry" "act" "entryRelationship" "observation" "code[@code='1020000003']/../"
	 "value"))

(defun common-section (entry)
  (mapcar (lambda (f) (funcall f entry))
	  (list #'common-level #'common-stage #'common-course-name)))

(defun first-section-init (entry)
  (xpath (node-set->dom)
	 entry
	 "ClinicalDocument" "component" "structuredBody" "component" "section/u:code[@code='90030']/../"
	 "entry" "act"))

(defun first-section (entry)
  (let1 v (first-section-init entry)
    (if v
	(list (get-elements v "code" ("text/code"))
	      (get-elements v "effectiveTime" ("text/value"))
	      (get-elements v "performer" "assignedEntity" "code" ("text/code"))
	      (get-elements v "entryRelationship" "observation" "effectiveTime" "width" ("text/value")))
	(make-list 4 :initial-element ""))))

(defun final-section-init (entry)
  (xpath (node-set->dom)
	 entry
	 "ClinicalDocument" "component" "structuredBody" "component" "section/u:code[@code='90060']/../"
	 "entry" "act"))

(defun final-section (entry)
  (let1 v (final-section-init entry)
    (if v
	(list (get-elements v "code" ("text/code"))
	      (get-elements v "effectiveTime" ("text/value"))
	      (get-elements v "performer" "assignedEntity" "code" ("text/code")))
	(make-list 3 :initial-element ""))))

(defun entry->test (entry)
  (xpath (node-set->dom)
	 entry
	 "ClinicalDocument" "recordTarget"))

;; base-UI
(defun path-to-dom (pathname)
  (cxml:parse-file pathname (cxml-dom:make-dom-builder)))

(defun purify (string)
  (coerce
   (remove-if (lambda (ch) (char-equal ch #\zero_width_no-break_space))
	      (coerce string 'list))
   'string))

(defun entry->string (entry)
  (& purify
     sb-ext:octets-to-string
     zip:zipfile-entry-contents
     entry))

(defun entry->dom (entry)
  (cxml:parse (entry->string entry)
	      (cxml-dom:make-dom-builder)))

(defparameter x1 #P"f:/20130628/hsido/2620700027_00263129_201210020_2.zip")
(defparameter x2 #P"f:/20130628/hsido/2614102230_00263129_201304260_2.zip")

(defun zip-parse-core (entry)
  (append (patient-id entry)
  	  (patient entry)
  	  (list (jnumber entry))
  	  (hospital entry)
  	  (list (occur-day entry))
  	  (common-section entry)
  	  (first-section entry)
  	  (final-section entry)))

(defun zip-parse (zipfile)
  (let1 n nil
    (zip:with-zipfile (z zipfile)
      (zip:do-zipfile-entries (key entry z)
	(if (ppcre:scan "DATA/[gh]" key)
	    (push (append (list (cond
				  ((ppcre:scan "DATA/g" key) :hsido)
				  ((ppcre:scan "DATA/h" key) :kensin)
				  (t :other))
				zipfile key)
			  (zip-parse-core entry))
		  n))))
    n))



(in-package :cl-user)
