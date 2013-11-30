(in-package :xrep)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (import '(cl-test-more:is
	    cl-test-more:is-expand
	    cl-test-more:ok)))

(is (create-xpath '("ClinicalDocument" "recordTarget" "patientRole" "id[@root='1.2.392.200119.6.101']"))
    "/u:ClinicalDocument/u:recordTarget/u:patientRole/u:id[@root='1.2.392.200119.6.101']"
    "function:create-xpath")

(is (create-xpath '("ClinicalDocument" "recordTarget" "patientRole" "id" ("value")))
    "/u:ClinicalDocument/u:recordTarget/u:patientRole/u:id/@value"
    "function:create-xpath")

(is-expand (xml-claims-xpath "checkupClaim" "subjectPerson" "insuranceCard" "insurerNumber")
	   (xpath:with-namespaces (("u" "http://tokuteikenshin.jp/checkup/2007"))
	     (xpath-to-node-list
	      (xpath:evaluate "/u:checkupClaim/u:subjectPerson/u:insuranceCard/u:insurerNumber"
			      documents)))
	   "Macro:xml-claims-xpath01")

(is-expand (xml-claims-xpath "checkupClaim")
	   (xpath:with-namespaces (("u" "http://tokuteikenshin.jp/checkup/2007"))
	     (xpath-to-node-list
	      (xpath:evaluate "/u:checkupClaim"
			      documents)))
	   "Macro:xml-claims-xpath02")

(is (reflect-directory-name #P"f:/zip/MAIN/2012/2613400288_00263129_201308280_1.zip"
					 "H24.07.14" "2")
		 "2613400288_00263129_201207142_1"
		 "function:reflect-directory-name"
		 :test #'equal)

(is (refine-xmlname-name "h26196001542013032901000001.xml"
			 "H25.1.1" "9")
    "h26196001542013010191000001.xml"
    "function:refine-xmlname-name")

(is (refine-xmlname-name "h26196001542013032901000001.xml"
			 "2013/1/1" "8")
    "h26196001542013010181000001.xml"
    "function:refine-xmlname-name")

(is (refine-xmlname
     "2619600154_92699024_201303290_1/DATA/h26196001542013032901000001.xml"
     "20130830" "1")
    "2619600154_00263129_201308301_1/DATA/h26196001542013083011000001.xml"
    "function:refine-xmlname"
    :test #'equal)

(is (nendo-end-8 "99") "21000331"
    "function:nendo-end-8")

(is-expand
 (defclass% XML ()
   (pathname	path-of)
   (jnumber	jnum-of)
   (documents	docs-of))
 (defclass XML ()
   ((pathname  :accessor path-of :initarg :pathname)
    (jnumber   :accessor jnum-of :initarg :jnumber)
    (documents :accessor docs-of :initarg :documents)))
 "Maco:defclass%")

(is (pathname-parse "2613400288_00263129_201308280_1")
    (values 2613400288 00263129 201308280 1)
    "function:pathname-parse")

