(in-package :xrep)

(defvar zip-program "f:/UnxUtils/usr/local/wbin/zip.exe")
(defvar refine-name-regexp "([hc])(\\d{10})(\\d{8})(\\d)(\\d)(\\d{6}).xml")
(defvar refine-zip-regexp  "([0-9_]+?)/(.+?)/(.+)")

(defun entry-to-stp (entry)
  (kensin::kx/parse-base entry))

(defmacro irr-match (binding string &optional then else)
  `(cl-irregsexp:if-match-bind ,binding ,string ,then ,else))

(defun date-string-normalization (xpath)
  (normal-date->string
   (strdt (xpath:string-value xpath))))

(defun xpath-to-node-list (xpath)
  (& car xpath::pipe-of xpath))

(defun stp-to-string (stp)
  (cl-irregsexp:match-replace-all
      (with-output-to-string (i)
	(stp:serialize stp (cxml:make-character-stream-sink i)))
    ("&#13;" "")))

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

;;; Name Utility
(defun refine-xmlname-name (name date branch)
  (declare (type string name) (optimize (speed 3) (safety 0)))
  (if (ppcre:scan refine-name-regexp name)
      (util::rxmatch-bind
       refine-name-regexp
       name
       (htype hcode predate prebranch ot base)
       (declare (ignorable predate prebranch))
       (format nil "~A~A~A~A~A~A.xml"
	       htype hcode (util::date-8 (strdt date)) branch ot base))
      name))

(defun refine-xmlname-zipname (zipname date branch)
  (declare (type string zipname date branch) (optimize (speed 3) (safety 0)))
  (util::rxmatch-bind
   "(\\d{10})_\\d{8}_(\\d{8})(\\d)_(\\d)"
   zipname
   (hcode predate prebranch ot)
   (format nil "~A_00263129_~A~A_~A"
	   hcode (util::date-8 (strdt date))
	   branch ot)))

(defun refine-xmlname (xmlname date branch)
  (declare (type string xmlname date branch)
  	   (optimize (speed 3) (safety 0)))
  (if (ppcre:scan refine-zip-regexp xmlname)
      (util::rxmatch-bind refine-zip-regexp xmlname
			  (zipname type name)
			  (format nil "~A/~A/~A"
				  (refine-xmlname-zipname zipname date branch)
				  type
				  (refine-xmlname-name name date branch)))
      (let1 list (ppcre:split "/" xmlname)
	(format nil "~A/~A"
		(refine-xmlname-zipname (first list) date branch)
		(second list)))))

(defmacro xpath-parse (target func &rest string)
  `(xpath:with-namespaces (("u" "urn:hl7-org:v3"))
     (funcall
      ,func
      (xpath:evaluate
       (create-xpath ',string)
       ,target))))

;; #P"f:/zip/2619600154_92699024_201303290_1.zip"
;; (defparameter testf #P"f:/zip/2619600154_92699024_201303290_1.zip")

(defun classifying (zipname)
  (let (h c ix other otr)
    (zip:with-zipfile (z zipname)
      (zip:do-zipfile-entries (key entry z)
	(declare (type string key))
	(cond
	  ((ppcre:scan "ix08_V08.xml$" key)
	   (push (list key (entry-to-stp entry)) ix))
	  ((ppcre:scan "CLAIMS/c\\d{26}.xml$" key)
	   (push (list key (entry-to-stp entry)) c))
	  ((ppcre:scan "DATA/h\\d{26}.xml$" key)
	   (push (list key (entry-to-stp entry)) h))
	  ((ppcre:scan "scc$" key)
	   (push key otr))
	  (t (push (list key (entry-to-stp entry)) other)))))
    (values h c ix other)))

;; (nendo-end-8 "12") => "20130331"
(defun nendo-end-8 (string2)
  (let1 endday (util::nendo-end (+ 2000 (read-from-string string2)))
    (format nil "~A~2,'0d~2,'0d"
	    (date-year endday) (date-month endday) (date-day endday))))

(defun participant-element-sexp (jnumber)
  (with-output-to-string (str)
    (cl-who:with-html-output (s str)
      (:participant
       :typeCode "HLD"
       (:functionCode :code "1" :codeSystem "1.2.392.200119.6.208")
       (:time (:high :value (nendo-end-8 (subseq jnumber 0 2))))
       (:associatedEntity :classCode "IDENT"
			  (:id :root "1.2.392.200119.6.209.100263129"
			       :extension jnumber)
			  (:scopingOrganization (:id :root "1.2.392.200119.6.101"
						     :extension "00263129")))))))

(defun participant-element (jnumber)
  (let* ((participant  (stp:make-element "participant" "urn:hl7-org:v3"))
	 (functioncode (stp:make-element "functionCode" "urn:hl7-org:v3"))
	 (time (stp:make-element "time" "urn:hl7-org:v3"))
	 (high (stp:make-element "high" "urn:hl7-org:v3"))
	 (ae  (stp:make-element "associatedEntity" "urn:hl7-org:v3"))
	 (aei (stp:make-element "id" "urn:hl7-org:v3"))
	 (asi (stp:make-element "id" "urn:hl7-org:v3"))
	 (so  (stp:make-element "scopingOrganization" "urn:hl7-org:v3")))
    (stp:add-attribute
     functioncode	(stp:make-attribute "1" "code"))
    (stp:add-attribute
     functioncode	(stp:make-attribute "1.2.392.200119.6.208" "codeSystem"))
    (stp:add-attribute
     high		(stp:make-attribute (nendo-end-8 (subseq jnumber 0 2)) "value"))
    (stp:add-attribute
     ae			(stp:make-attribute "IDENT" "classCode"))
    (stp:add-attribute
     aei		(stp:make-attribute "1.2.392.200119.6.209.100263129" "root"))
    (stp:add-attribute
     aei		(stp:make-attribute jnumber "extension"))
    (stp:add-attribute
     asi		(stp:make-attribute "1.2.392.200119.6.101" "root"))
    (stp:add-attribute
     asi		(stp:make-attribute "00263129" "extension"))
    (stp:append-child participant functioncode)
    (stp:append-child participant time)
    (stp:append-child participant ae)
    (stp:append-child time high)
    (stp:append-child ae aei)
    (stp:append-child ae so)
    (stp:append-child so asi)
    ;; (stp:make-document participant)
    participant))

;; (defun participant-element (jnumber)
;;   (stp:document-element
;;    (cxml:parse (participant-element-sexp jnumber)
;; 	       (stp:make-builder))))

(defun checkup-card-sexp (jnumber)
  (with-output-to-string (str)
    (cl-who:with-html-output (s str)
      (:checkupCard
       (:id :root "1.2.392.200119.6.209" :extension jnumber)
       (:effectiveTime (:high :value (nendo-end-8 (subseq jnumber 0 2))))
       (:chargeTypeBasic :code "1")
       (:chargeTypeDetail :code "1")))))

(defun checkup-card (jnumber)
  (let ((cc (stp:make-element "checkupCard" "http://tokuteikenshin.jp/checkup/2007"))
  	(ci (stp:make-element "id" "http://tokuteikenshin.jp/checkup/2007"))
  	(et (stp:make-element "effectiveTime" "http://tokuteikenshin.jp/checkup/2007"))
  	(hi (stp:make-element "high" "http://tokuteikenshin.jp/checkup/2007"))
  	(ct (stp:make-element "chargeTypeBasic" "http://tokuteikenshin.jp/checkup/2007")))
    (stp:append-child cc ci)
    (stp:append-child cc et)
    (stp:append-child cc ct)
    (stp:append-child et hi)
    (stp:add-attribute ci (stp:make-attribute "1.2.392.200119.6.209" "root"))
    (stp:add-attribute ci (stp:make-attribute jnumber "extension"))
    (stp:add-attribute hi (stp:make-attribute (nendo-end-8 (subseq jnumber 0 2)) "value"))
    (stp:add-attribute ct (stp:make-attribute "1" "code"))
    cc)
  ;; (stp:document-element
  ;;  (cxml:parse (checkup-card-sexp jnumber)
  ;; 	       (stp:make-builder)))
  )

(defmacro defclass% (classname superclass &rest clause)
  `(defclass ,classname ,superclass
     ,(mapcar
       (lambda (l) `(,(first l) :accessor ,(second l)
		      :initarg ,(alexandria:make-keyword (first l))))
       clause)))

(defclass% XML ()
  (pathname	path-of)
  (jnumber	jnum-of)
  (documents	docs-of))

(defclass% XML-DATA   (XML)
  (crpie	crpie-of)
  (cpair	cpair-of)
  (cpasie	cpasie-of)
  (cev		cev-of)
  (catv		catv-of)
  (date		date-of)
  (branch	branch-of))

(defmacro fix-attribute (parent-node attribute value)
  (let ((att (gensym)))
  `(stp:with-attributes ((,att ,attribute)) ,parent-node
     (setf ,att ,value) ,parent-node)))

(defmacro document-rebuild! (jnumber)
  `(setq documents
	 (stp:document
	  (stp:append-child (stp:document-element documents)
			    (participant-element ,jnumber)))))

(defmethod initialize-instance :after ((xd XML-DATA) &rest args)
  (declare (ignorable args))
  (with-slots (pathname documents crpie cpair cpasie jnumber
			catv cev date branch) xd
    (setq pathname	(if date 
			    (refine-xmlname pathname date branch)
			    (format nil "~A" pathname))
	  crpie		(xpath-parse documents #'xpath-to-node-list
				     "ClinicalDocument" "recordTarget" "patientRole"
				     "id[@root='1.2.392.200119.6.101']")
	  cpair		(xpath-parse documents #'xpath-to-node-list
				     "ClinicalDocument" "participant" "associatedEntity"
				     "id")
	  cpasie	(xpath-parse documents #'xpath-to-node-list
				     "ClinicalDocument" "participant" "associatedEntity"
				     "scopingOrganization"
				     "id[@root='1.2.392.200119.6.101']")
	  cev		(xpath-parse documents #'xpath-to-node-list
				     "ClinicalDocument" "effectiveTime")
	  catv		(xpath-parse documents #'xpath-to-node-list
				     "ClinicalDocument" "author" "time"))
    (if cpair
	(progn
	  (fix-attribute cpair "root" "1.2.392.200119.6.209.100263129")
	  (if jnumber
	      (fix-attribute cpair "extension" jnumber)))
    	(if jnumber
	    (document-rebuild! jnumber)))
    (if date
	(progn
	  (fix-attribute cev "value" (util::date-8 (strdt date)))
	  (fix-attribute catv "value" (util::date-8 (strdt date)))))))

(defclass% XML-CLAIMS (XML)
  (csiie	csiie-of)
  (cci		cci-of)
  (root		root-of)
  (date		date-of)
  (branch	branch-of))

(defmacro claims-document-rebuild! (jnumber)
  `(stp:replace-child root
		     ;; old-child
		     (stp:find-child-if
		      (lambda (o) (and (typep o 'stp:element)
				       (equal "checkupCard" (stp:local-name o))))
		      root)
		     ;; new-child
		     (checkup-card ,jnumber)))

(defmacro xml-checkup-xpath (&rest string-list)
  `(xpath:with-namespaces (("u" "http://tokuteikenshin.jp/checkup/2007"))
     (xpath-to-node-list
      (xpath:evaluate ,(format nil "~{/u:~A~}" string-list)
		      documents))))

(defmethod initialize-instance :after ((xc XML-CLAIMS) &rest args)
  (declare (ignorable args))
  (with-slots (pathname documents csiie cci jnumber root date branch) xc
    (setq pathname	(if date
			    (refine-xmlname pathname date branch)
			    (format nil "~A" pathname))
	  csiie		(xml-checkup-xpath "checkupClaim" "subjectPerson" "insuranceCard" "insurerNumber")
	  cci		(xml-checkup-xpath "checkupClaim" "checkupCard" "id")
	  root		(xml-checkup-xpath "checkupClaim"))
    (fix-attribute csiie "extension" "00263129")
    (if jnumber
	(if cci
	    (fix-attribute cci "extension" jnumber)
	    (claims-document-rebuild! jnumber)))))

(defclass% XML-IX     (XML)
  (iire		iire-of)
  (date		date-of)
  (branch	branch-of)
  (icv		icv-of))

(defmethod initialize-instance :after ((xi XML-IX) &rest args)
  (declare (ignorable args))
  (with-slots (pathname documents iire date branch icv) xi
    (setq pathname	(if date
			    (refine-xmlname pathname date branch)
			    (format nil "~A" pathname))
	  iire		(xml-checkup-xpath "index" "receiver" "id")
	  icv		(xml-checkup-xpath "index" "creationTime"))
    (fix-attribute iire "extension" "00263129")
    (if date
	(fix-attribute icv "value" date))))

(defclass% XML-OTHER (XML)
  (date		date-of)
  (branch	branch-of))

(defmethod initialize-instance :after ((xo XML-OTHER) &rest args)
  (declare (ignorable args))
  (with-slots (pathname documents date branch) xo
    (setq pathname	(if date
			    (refine-xmlname pathname date branch)
			    (format nil "~A" pathname)))))

(defclass% XML-OTR (XML))


;;; Utility
(defun pathname-parse (zip)
  (declare (type string zip))
  (irr-match 
   ((hcode (integer :length 10)) "_"
    (ig1   (integer :length 8)) "_"
    (date  (integer :length 8)) (sub1 (integer)) "_" (sub2 (integer)) (last))
   zip
   (values hcode date sub1 sub2)))

(defun true-name (directory)
  (multiple-value-bind (hcode date sub1 sub2)
      (pathname-parse directory)
    (format nil "~A_00263129_~A~A_~A"
	    hcode date sub1 sub2)))

(defun true-zipname (zip) (true-name (pathname-name zip)))

(defun true-xmlname (xmlname)
  (let1 list (cl-irregsexp:match-split #\/ xmlname)
    (format nil "~{~A~^/~}"
	    (cons (true-name (car list))
		  (cdr list)))))

(defmethod xmlprint ((x XML) op)
  (format op "~A" (stp-to-string (docs-of x))))

(defmethod xmlwrite ((x XML))
  (call-with-output-file2 (true-xmlname (path-of x))
    (lambda (op) (xmlprint x op))))

(defmethod xmlprint ((x XML-OTR) op)
  nil)

(defmethod xmlprint ((x STRING) op)
  (write-line x op))

;;; ((2 "12120.....") (8 "12150....."))
;;; => (nil "12120....." nil nil nil nil nil "12150.....")
(defun make-jusinken-list (xmls js)
  (if (eq js 'nil)
      (make-list (length xmls) :initial-element nil)
      (iter (with l = (make-list (length xmls) :initial-element nil))
	    (for pair :in js)
	    (setf (nth (1- (first pair)) l) (second pair))
	    (finally (return l)))))

(defun create-data (dataxml jnumbers date branch)
  (iter (with jusinken = (make-jusinken-list dataxml jnumbers))
	(for data :in (sort2 dataxml string< first))
	(for counter :upfrom 0)
	(collect (make-instance 'XML-DATA
				:pathname (first data)
				:documents (second data)
				:jnumber (nth counter jusinken)
				:date date
				:branch branch))))

(defun create-claims (claimsxml jnumbers date branch)
  (iter (with jusinken = (make-jusinken-list claimsxml jnumbers))
	(for data :in (sort2 claimsxml string< first))
	(for counter :upfrom 0)
	(collect (make-instance 'XML-CLAIMS
				:pathname (first data)
				:documents (second data)
				:jnumber (nth counter jusinken)
				:date date
				:branch branch))))

(defun xml-instance (zipname jusinken date branch)
  (multiple-value-bind (h c ix other otr) (classifying zipname)
    (append
     (create-data h jusinken date branch)
     (create-claims c jusinken date branch)
     (mapcar (^ (o) (make-instance 'XML-IX
				   :pathname (first o) :documents (second o)
				   :date date  :branch branch)) ix)
     (mapcar (^ (o) (make-instance 'XML-OTHER
				   :pathname (first o) :documents (second o)
				   :date date :branch branch)) other))))

(defun zipfile-store (zip)
  (cl-fad:copy-file
   zip (make-pathname :defaults zip :type "zip.rep")))

(defmacro reflect-make-directory (directory)
  `(progn
     (util::make-directory ,directory)
     ,@(mapcar (lambda (d) `(util::make-directory (format nil "~A/~A" ,directory ,d)))
	       `("XSD/" "XSD/coreschemas/" "DATA/" "CLAIMS/"))))

(defmacro reflect-make-instance (zipfile jusinken date branch)
  `(iter (for xml :in (xml-instance ,zipfile ,jusinken ,date ,branch))
	 ;; (call-with-output-file2 (true-xmlname (path-of xml))
	 ;;   (lambda (op) (xmlprint xml op)))
	 (xmlwrite xml)))


(defun reflect-zip-program (directory)
  (sb-ext:run-program zip-program
		      `("-r" "-D"
			     ,(format nil "~A.zip" directory)
			     ,directory)))
;;; 呼び方
;;; --------------------------------------------------
;;; 2人目と8人目の受診券をそれぞれ直したい場合、
;;; (reflect "zip名" ((2 "121123...") (8 "1250......")))
(defmacro reflect (zip &optional jusinken)
  "2人目と8人目の受診券をそれぞれ直したい場合、
 (reflect \"zip名\" ((2 \"121123...\") (8 \"1250......\")))"
  (let ((truename (true-zipname zip)))
    `(progn
       (reflect-make-directory ,truename)
       (reflect-make-instance ,zip ',jusinken nil nil)
       (reflect-zip-program ,truename)
       (cl-fad:delete-directory-and-files ,truename))))

(defun reflect-directory-name (zip date branch)
  (cl-irregsexp:if-match-bind
   ((hcode (string 10)) "_" (insurer (string 8)) "_" (string 9) "_" (br (string)))
   (the string (pathname-name zip))
   (format nil "~A_00263129_~A~A_~A"
	   hcode (util::date-8 (strdt date)) branch br)))

(defun reflect-rebuild (zipname)
  (zip:unzip zipname "./")
  (reflect-zip-program (pathname-name zipname))
  (cl-fad:delete-directory-and-files (pathname-name zipname)))

(defun reflect-directory (zip date branch)
  (let ((truename (reflect-directory-name zip date branch)))
    (reflect-make-directory truename)
    (reflect-make-instance zip nil date branch)
    (reflect-zip-program truename)
    (cl-fad:delete-directory-and-files truename)))

(defun reflect-pathname-type-change (basename)
  "指定されたファイル名(ベースネーム)から、保存場所にあるファイルの拡張子を
'zip'から'zipq'に変更し、検索にかからないようにする。"
  (let* ((path   (make-pathname :name basename :type "zip"))
	 (occurd (last1 (car (kensin::kx/parse path))))
	 (nendo  (nendo-year (strdt occurd)))
	 (gen    (make-pathname :directory `(:absolute "zip" "MAIN" ,(write-to-string nendo))
				:name basename
				:type "zip")))
    (rename-file (make-pathname :defaults gen :device "d")
		 (make-pathname :defaults gen :device "d" :type "zipq"))
    (rename-file (make-pathname :defaults gen :device "f")
		 (make-pathname :defaults gen :device "f" :type "zipq"))))

;; (kensin::kx/parse #P"2619700095_00263129_201206130_1.zip")

;; (reflect-directory #P"d:/zip/2610306868_92699024_201304090_1.zip"
;; 		   "20130409" "0")

;; (reflect-directory #P"f:/zip/2610503118_92699024_201307310_1.zip" "20130731" "1")

;; (reflect "2610306868_00263129_201304090_1.zipq"
;; 	 ((1 "13185141101")))
