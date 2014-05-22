;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (import '(;; alexandria:read-file-into-string
;; 	    ;; ppcre:scan
;; 	    ;; ppcre:regex-replace-all
;; 	    ;; sb-impl::fd-stream-file
;; 	    ;; stp:make-builder
;; 	    zip::zipfile-entry-stream)))
(in-package :kensin)

(defun kx/time-string (xpath)
  (normal-date->string
   (string->date (xpath:string-value xpath))))

;; (defun singlep (list)
;;   (and (car list) (not (cdr list))))

(defun kx/create-xpath (string-list)
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

(kx/create-xpath '("ClinicalDocument" "participant" "associatedEntity" "id"
		   ("root" "1.2.392.200119.6.210.100263129") ("extension")))

(defmacro kx/xpath (target func &rest string)
  `(xpath:with-namespaces (("u" "urn:hl7-org:v3"))
     (funcall
      ,func
      (xpath:evaluate
       (kx/create-xpath ',string)
       ,target))))

(defun kx/jusinken (doc)
  (kx/xpath doc
	    #'xpath:string-value
	    "ClinicalDocument" "participant" "associatedEntity" "id" ("extension")))

(defun hs/jusinken (doc)
  (kx/xpath doc
	    #'xpath:string-value
	    "ClinicalDocument" "participant" "associatedEntity"
	    "id" ("root" "1.2.392.200119.6.210.100263129") ("extension")))

(defun hs/support-level (doc)
  (kx/xpath doc
	    (lambda (o)
	      (string-case (xpath:string-value o)
		("1" "積極的支援")
		("2" "動機づけ支援")
		("3" "その他")
		("4" "その他")))
	    ;#'xpath:all-nodes
	    "ClinicalDocument" "component" "structuredBody" "component"
	    "section" "entry" "act" "entryRelationship" "observation" "value"
	    ("codeSystem" "1.2.392.200119.6.3001") ("code")))

(defun hs/change-act-stage (doc)
  (kx/xpath doc
	    (lambda (o)
	      (string-case (xpath:string-value o)
		("1" "意志なし")
		("2" "意志あり(6か月以内)")
		("3" "意志あり(近いうち)")
		("4" "取組み済み(6か月未満)")
		("5" "取組み済み(6か月以上)")))
	    "ClinicalDocument" "component" "structuredBody" "component"
	    "section" "entry" "act" "entryRelationship" "observation" "value"
	    ("codeSystem" "1.2.392.200119.6.3002") ("code")))

(defun hs/course-name (doc)
  (kx/xpath doc
	    #'xpath:string-value
	    "ClinicalDocument/u:component/u:structuredBody/u:component/u:section/u:entry/u:act/u:entryRelationship/u:observation[u:code/@code='1020000003']/u:value"))

(defun hs/first-date (doc)
  (kx/xpath doc
	    #'xpath:string-value
	    ;#'xpath:all-nodes
	    "ClinicalDocument" "component" "structuredBody" "component"
	    "section" "entry" "act" "effectiveTime" ("value")))

(defun hs/performer (doc)
  (kx/xpath doc
	    (lambda (o)
	      (string-case (xpath:string-value o)
		("1" "医師")
		("2" "保健師")
		("3" "管理栄養士")
		("4" "その他")))
	    ;#'xpath:all-nodes
	    "ClinicalDocument" "component"
	    "structuredBody" "component" "section" "entry"
	    "act" "performer" "assignedEntity" "code" ("code")))

(defun hs/final-date (doc)
  (kx/xpath doc
	    #'xpath:string-value
	    ;#'xpath:all-nodes
	    "ClinicalDocument" "component"
	    "structuredBody" "component" 
	    "section[u:code/@code='90060']" "entry" "act" "effectiveTime" ("value")))

(defun hs/span-attributes (doc)
  (cxml-stp:map-attributes
   'list
   #'xpath:string-value
   (car (xpath:all-nodes doc))))

(defun hs/span (doc)
  (kx/xpath doc
	    (lambda (attributes)
	      (if attributes
		  (destructuring-bind (ig1 value unit) (hs/span-attributes attributes)
		    (declare (ignorable ig1))
		    (format nil "~A~A" value unit))
		  ""))
	    "ClinicalDocument" "component" "structuredBody" "component"
	    "section" "entry" "act" "entryRelationship"
	    "observation[u:code/@code='1021000020']" "value"))

(defun kx/kigobango-set (doc)
  (mapcar
   #'xpath:string-value
   (xpath:all-nodes
    (kx/xpath doc
		#'identity
		"ClinicalDocument" "recordTarget" "patientRole" "id" ("extension")))))

(defun kx/patient (doc)
  (kx/xpath doc
	    #'identity
	    "ClinicalDocument"
	    "recordTarget"
	    "patientRole"
	    "patient"))

(defun kx/name (doc)
  (kx/xpath doc #'xpath:string-value
	    "ClinicalDocument" "recordTarget" "patientRole" "patient" "name"))

(defun kx/birthday (doc)
  (kx/xpath doc #'kx/time-string
	    "ClinicalDocument" "recordTarget" "patientRole" "patient" "birthTime" ("value")))

(defun kx/occur-day (doc)
  (kx/xpath doc #'kx/time-string
	      "ClinicalDocument" "documentationOf" "serviceEvent" "effectiveTime" ("value")))

(defun kx/create-day (doc)
  (kx/xpath doc #'kx/time-string
	    "ClinicalDocument" "effectiveTime" ("value")))

(defun kx/hospital-code (doc)
  (kx/xpath doc #'xpath:string-value
	      "ClinicalDocument" "documentationOf" "serviceEvent"
	      "performer" "assignedEntity" "representedOrganization"
	      "id" ("extension")))

(defun kx/hospital-name (doc)
  (kx/xpath doc #'xpath:string-value
	      "ClinicalDocument" "documentationOf" "serviceEvent"
	      "performer" "assignedEntity" "representedOrganization" "name"))

(defmacro kx/value-parse (id doc)
  `(kx/xpath ,doc #'xpath:string-value
	     "ClinicalDocument" "component" "structuredBody"
	     "component" "section" "entry" "observation"
	     ,(format nil "code[@code='~A']/.." id)
	     "value" ("value")))

(defmacro kx/relative-value-parse (id doc)
  `(kx/xpath ,doc #'xpath:string-value
	     "ClinicalDocument" "component" "structuredBody"
	     "component" "section" "entry" "observation"
	     "entryRelationship" "observation"
	     ,(format nil "code[@code='~A']/.." id)
	     "value" ("value")))

(defmacro kx/code-parse (id doc)
  `(kx/xpath ,doc #'xpath:string-value
	     "ClinicalDocument" "component" "structuredBody"
	     "component" "section" "entry" "observation"
	     ,(format nil "code[@code='~A']/.." id)
	     "value" ("code")))

(defmacro kx/text-parse (id doc)
  `(kx/xpath ,doc #'xpath:string-value
	     "ClinicalDocument" "component" "structuredBody"
	     "component" "section" "entry" "observation"
	     "entryRelationship" "observation"
	     ,(format nil "code[@code='~A']/.." id)
	     "value"))

(defmacro string-not-null-or (&rest args)
  (let ((sym (gensym)))
    (if args
	`(let1 ,sym ,(car args)
	   (if (string-not-null ,sym)
	       ,sym
	       (string-not-null-or ,@(cdr args))))
	nil)))

(defmacro sor (&rest args)
  `(string-not-null-or ,@args))

(defun kx/height	(doc) (kx/value-parse "9N001000000000001" doc))
(defun kx/weight	(doc) (kx/value-parse "9N006000000000001" doc))
(defun kx/bmi		(doc) (kx/value-parse "9N011000000000001" doc))
(defun kx/腹囲		(doc) (kx/value-parse "9N016160100000001" doc))
(defun kx/既往歴	(doc) (kx/code-parse "9N056000000000011" doc))
(defun kx/自覚症状	(doc) (kx/code-parse "9N061000000000011" doc))
(defun kx/他覚症状	(doc) (kx/code-parse "9N066000000000011" doc))
(defun kx/収縮期血圧	(doc) (kx/value-parse "9A751000000000001" doc))
(defun kx/拡張期血圧	(doc) (kx/value-parse "9A761000000000001" doc))
(defun kx/中性脂肪	(doc) (kx/value-parse "3F015000002327101" doc))
(defun kx/HDL		(doc) (kx/value-parse "3F070000002327101" doc))
(defun kx/LDL		(doc) (kx/value-parse "3F077000002327101" doc))
(defun kx/GOT		(doc) (kx/value-parse "3B035000002327201" doc))
(defun kx/GPT		(doc) (kx/value-parse "3B045000002327201" doc))
(defun kx/GTP		(doc) (kx/value-parse "3B090000002327101" doc))
(defun kx/血糖		(doc) (sor (kx/value-parse "3D010000001927201" doc)
				   (kx/value-parse "3D010000001926101" doc)))
(defun kx/hba1c		(doc) (sor (kx/value-parse "3D045000001920402" doc)
				   (kx/value-parse "3D045000001906202" doc)
				   (kx/value-parse "3D045000001999902" doc)))
(defun kx/尿糖		(doc) (kx/code-parse "1A020000000191111" doc))
(defun kx/尿蛋白	(doc) (kx/code-parse "1A010000000191111" doc))
(defun kx/ヘマトクリット	(doc) (kx/relative-value-parse "2A040000001930102" doc))
(defun kx/血色素	(doc) (kx/relative-value-parse "2A030000001930101" doc))
(defun kx/赤血球数	(doc) (kx/relative-value-parse "2A020000001930101" doc))
;; (defun kx/心電図有無	(doc) (kx/value-parse "9A110160700000011" doc))
(defun kx/心電図所見	(doc) (kx/text-parse "9A110160800000049" doc))
(defun kx/眼底検査	(doc) (kx/text-parse "9E100160900000049" doc))
(defun kx/メタボ	(doc) (kx/code-parse "9N501000000000011" doc))
(defun kx/指導レベル	(doc) (kx/code-parse "9N506000000000011" doc))
;(defun kx/医師の診断	(doc) (kx/value-parse "" doc))
;(defun kx/医師		(doc) (kx/value-parse "" doc))
(defun kx/服薬血圧	(doc) (kx/code-parse "9N701000000000011" doc))
(defun kx/服薬血糖	(doc) (kx/code-parse "9N706000000000011" doc))
(defun kx/服薬脂質	(doc) (kx/code-parse "9N711000000000011" doc))
(defun kx/喫煙		(doc) (kx/code-parse "9N736000000000011" doc))

(defun kx/医師の診断 (doc)
  (kx/xpath doc #'xpath:string-value
	    "ClinicalDocument" "component" "structuredBody"
	    "component" "section" "entry" "observation"
	    "code[@code='9N511000000000049']/.." "value"))

(defun kx/医師 (doc)
  (kx/xpath doc #'xpath:string-value
	    "ClinicalDocument" "component" "structuredBody"
	    "component" "section" "entry" "observation"
	    "code[@code='9N511000000000049']/.." "author"
	    "assignedAuthor" "assignedPerson" "name"))

(defun kx/directory (pathname)
  (nth 1 (reverse (pathname-directory pathname))))

(defun kx/basename (pathname)
  (format nil "~A.~A"
	  (pathname-name pathname)
	  (pathname-type pathname)))

;; (cxml:parse (alexandria:read-file-into-string "f:/xml/data/2610200251_00263129_201103100_1/2610200251_00263129_201103100_1/DATA/h26102002512011031001000001.xml")
;; 	    (stp:make-builder))

(defun kx-string-to-stp (string)
  (cxml:parse string (stp:make-builder)))

(defun kx-path-to-stp (pathname)
  (kx-string-to-stp (alexandria:read-file-into-string pathname)))

(defun kx-stream-to-stp (stream)
  (kx-string-to-stp
   (coerce (loop :for i = (read-char stream nil nil nil)
	      :while i
	      :collect i)
	   'string)))

(defun kx-purify (string)
  (coerce
   (remove-if (lambda (ch) (char-equal ch #\zero_width_no-break_space))
	      (coerce string 'list))
   'string))

(defun kx/parse-base (entry)
  (& kx-string-to-stp
     kx-purify
     #+sbcl  sb-ext:octets-to-string
     #+clisp babel:octets-to-string
     zip:zipfile-entry-contents
     entry))

(defun kx/parse-core (entry)
  (kx/parse-main (kx/parse-base entry)))

(defun kx/parse-core2 (entry)
  (mapcar
   (lambda (f) (funcall f (kx/parse-base entry)))
   (list #'kx/name #'kx/birthday #'kx/occur-day #'kx/hospital-code
  	 #'kx/hospital-name #'kx/jusinken
  	 #'kx/height #'kx/weight #'kx/bmi #'kx/腹囲
  	 #'kx/既往歴 #'kx/自覚症状 #'kx/他覚症状
  	 #'kx/収縮期血圧 #'kx/拡張期血圧 #'kx/中性脂肪
  	 #'kx/HDL #'kx/LDL #'kx/GOT #'kx/GPT
  	 #'kx/GTP #'kx/血糖 #'kx/hba1c 
  	 #'kx/尿糖 #'kx/尿蛋白 #'kx/ヘマトクリット #'kx/血色素
  	 #'kx/赤血球数 #'kx/心電図所見 ;#'kx/#'kx/
  	 #'kx/メタボ #'kx/指導レベル #'kx/医師の診断 #'kx/医師
  	 #'kx/服薬血圧 #'kx/服薬血糖 #'kx/服薬脂質 #'kx/喫煙))
  )

(defun kx/parse-main (stp)
  `(,(kx/jusinken stp)
     ,@(aif (kx/kigobango-set stp)
	    (if (<= 3 (length it))
		(cdr it)
		(list "" ""))
	    (list "" ""))
     ,(kx/name stp)
     ,(kx/birthday stp)
     ,(kx/hospital-code stp)
     ,(kx/hospital-name stp)
     ,(kx/create-day stp)
     ,(kx/occur-day stp)))

(defun kx/parse-main2 (entry zipname xmlname)
  (let1 stp (kx/parse-base entry)
    `(,zipname
      ,xmlname
      ,(kx/jusinken stp)
      ,@(aif (kx/kigobango-set stp)
	     (if (<= 3 (length it))
		 (cdr it)
		 (list "" ""))
	     (list "" ""))
      ,(kx/name stp)
      ,(kx/birthday stp)
      ,(kx/hospital-code stp)
      ,(kx/hospital-name stp)
      ,(kx/create-day stp)
      ,(kx/occur-day stp))))

(defun hs/parse-main (entry zipname xmlname)
  (let1 stp (kx/parse-base entry)
    `(,zipname
      ,xmlname
      ,(kx/jusinken stp)
      ,@(aif (kx/kigobango-set stp)
	     (if (<= 3 (length it))
		 (cdr it)
		 (list "" ""))
	     (list "" ""))
      ,(kx/name stp)
      ,(kx/birthday stp)
      ,(kx/hospital-code stp)
      ,(kx/hospital-name stp)
      ,(kx/create-day stp)
      ,(kx/occur-day stp)
      ,(hs/jusinken stp)
      ,(hs/support-level stp)
      ,(hs/change-act-stage stp)
      ,(hs/course-name stp)
      ,(hs/first-date stp)
      ,(hs/performer stp)
      ,(hs/final-date stp)
      ;; ,(hs/span stp)
      )))

(defun kx/find (predicate zipfile attach-fn)
  (rlet1 n nil
    (zip:with-zipfile (zip zipfile)
      (zip:do-zipfile-entries (key entry zip)
	(if (funcall predicate key entry)
	    (push (funcall attach-fn key entry) n))))))

(defun kx/parse-zip-main (zipfile)
  (rlet1 n nil
    (zip:with-zipfile (zip zipfile)
      (zip:do-zipfile-entries (key entry zip)
	(if (ppcre:scan "DATA/h" key)
	    (push (kx/parse-core entry) n))))))

(defun kx/parse-zip-main2 (zipfile)
  (rlet1 n nil
    (zip:with-zipfile (zip zipfile)
      (zip:do-zipfile-entries (key entry zip)
	(if (ppcre:scan "DATA/h" key)
	    (push (kx/parse-main2 entry
				  (hs/zipname entry)
				  (hs/xmlname entry)) n))))))

(defun kx/parse-zip-main3 (zipfile)
  (rlet1 n nil
    (zip:with-zipfile (zip zipfile)
      (zip:do-zipfile-entries (key entry zip)
	(if (ppcre:scan "DATA/h" key)
	    (push (kx/parse-core2 entry) n))))))

(defun hs/xmlname (entry)
  (last1
   (ppcre:split "/" (zip:zipfile-entry-name entry))))

(defun hs/zip-entry-stream-name (entry)
  (ppcre:regex-replace-all
   "\\\\"
   (sb-impl::fd-stream-file (zip::zipfile-entry-stream entry))
   "/"))

(defun hs/zipname (entry)
  (pathname-name
   (make-pathname :defaults (hs/zip-entry-stream-name entry))))

(defun hs/parse-zip-main (zipfile)
  (rlet1 n nil
    (zip:with-zipfile (zip zipfile)
      (zip:do-zipfile-entries (key entry zip)
	(if (ppcre:scan "DATA/g" key)
	    (push (hs/parse-main entry
				 (hs/zipname entry)
				 (hs/xmlname entry)) n))))))

(defun kx/parse (arg)
  (typecase arg
    (pathname
     (string-case (pathname-type arg)
       ("xml" (& kx/parse-main kx-path-to-stp arg))
       ("zip" (kx/parse-zip-main arg))))
    (string   (& kx/parse-main kx-string-to-stp arg))
    (stream   (& kx/parse-main kx-stream-to-stp arg))))

(defun kx/parse2 (zipname)
  (kx/parse-zip-main2 zipname))

(defun kx/parse3 (zipname)
  (kx/parse-zip-main3 zipname))

(defun kx/parse3-print (zipname)
  (let1 newname (make-pathname :name (pathname-name zipname)
			       :type "csv")
   (call-with-output-file2 newname
     (lambda (op)
       (format op "~{~A~^,~}~%"
	       (list "氏名" "生年月日" "受診日" "機関コード" "機関名"
		     "受診券整理番号" "身長" "体重" "BMI" "腹囲"
		     "既往歴" "自覚症状" "他覚症状" "収縮期血圧" "拡張期血圧"
		     "中性脂肪" "HDL" "LDL" "GOT" "GPT"
		     "GTP" "血糖" "hba1c" "尿糖" "尿蛋白"
		     "ヘマトクリット" "血色素" "赤血球数" "心電図所見" "メタボ"
		     "指導レベル" "医師の診断" "医師" "服薬血圧" "服薬血糖"
		     "服薬脂質" "喫煙"))
     (iter (for line :in (kx/parse3 zipname))
	   (format op "~{~A~^,~}~%" line)))
     :code :SJIS)))

(defmacro kx/alias (defname func)
  `(defmacro ,defname (&rest args)
     `(,,func ,@args)))


;; (kx/parse3-print #P"d:/zip/MAIN/2012/2610601342_00263129_201308080_1.zip")
;; (kx/parse3-print #P"d:/zip/MAIN/2012/2610307411_00263129_201303050_1.zip")
;; (kx/parse3-print #P"f:/zip/MAIN/2012/2613400288_00263129_201308130_1.zip")
;; (kx/alias hs/name		kx/name)
;; (kx/alias hs/birthday		kx/birthday)
;; (kx/alias hs/hospital-code	kx/hospital-code)
;; (kx/alias hs/hospital-name	kx/hospital-name)
;; (kx/alias hs/time-string	kx/time-string)
;; (kx/alias hs/create-xpath	kx/create-xpath)
;; (kx/alias hs/xpath		kx/xpath)
;; (kx/alias hs/occur-day		kx/occur-day)
;; (kx/alias hs/create-day		kx/create-day)
;; (kx/alias hs/directory		kx/directory)
;; (kx/alias hs/basename		kx/basename)
;; (kx/alias hs/parse-main		kx/parse-main)
;; (kx/alias hs/parse-core		kx/parse-core)

;(defparameter testf #P"f:/20130628/hsido/2614102230_00263129_201206150_2.zip")
;; (defparameter t2 #P"f:/zip/2610301430_00263129_201104080_1.zip")
;; (defparameter testf #P"f:/20130628/hsido/2614102230_00263129_201304260_2.zip")

;; (iter (for zip in (directory-list "./" :type "zip"))
;;       (iter (for line in (hs/parse-zip-main zip))
;; 	    (print (string-join line ","))))

