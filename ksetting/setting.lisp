(defpackage :kensin/setting
  (:nicknames :ksetting :ks)
  (:use :cl :util :iterate))

(in-package :ksetting)

(defmacro whichfile (pathname)
  `(or ,@(mapcar
	  (lambda (parent) `(cl-fad:file-exists-p
			     ,(make-pathname :defaults (format nil "~A~A" parent pathname))))
	  '(#P"C:/Users/Jumpei/20130628/"
	    #P"f:/20130628/"
	    #P"Y:/47伊東/util2/ksetting/"))))

(defparameter xmlfile (whichfile "setting.xml"))

(defun xml-content ()
  (cxml:parse-file xmlfile (cxml-dom:make-dom-builder)))

(defmacro defvar@ (&rest pair)
  `(let1 xmlcontent (xml-content)
     ,@(mapcar
	(lambda (p) `(defvar ,(first p)
		       (xpath:string-value (xpath:evaluate ,(second p) xmlcontent))))
	(group pair 2))))

(defvar@
  year			"/setting/year/@value"
  zipdir		"//zipdir"
  dock13-file		"//dock13-file"
  kfile-regexp		"//kfile-regexp"
  dock-directory	"//dock-directory"
  setcontract-file	"//setcontract-file"
  kclassify-topdir	"//kclassify-topdir"
  kanbo-property-file	"//kanbo-property-file"
  setcontract-file	"//setcontract-file"
  setcontract-csv	"//setcontract-csv"
  temporary-directory	"//temporary-directory")

(defmacro defvar-list@ (&rest pair)
  `(let1 xmlcontent (xml-content)
     ,@(mapcar
	(lambda (p) `(defvar ,(first p)
		       ;; (xpath:string-value (xpath:evaluate ,(second p) xmlcontent))
		       (xpath:map-node-set->list
			#'xpath:string-value
			(xpath:evaluate (format nil "~A/item" ,(second p))
					xmlcontent))))
	(group pair 2))))

(defvar-list@
  topdir		"//topdir"
  zenken-directory	"//zenken-directory"
  tokutei-directory	"//tokutei-directory"
  hsido-directory	"//hsido-directory")

(defparameter zenkenfile
  (xpath:map-node-set->list
   (lambda (o) (list (xpath:string-value (dom:get-attribute-node o "year"))
		     (xpath:string-value o)
		     (aif (dom:get-attribute-node o "fixed")
			  (if (equal "true" (xpath:string-value it)) :fixed nil)
			  nil)))
   (xpath:evaluate "//zenkenfile/item" (ks::xml-content))))

(defmacro server-file (file)
  `(merge-pathnames *server-directory* ,file))

(defvar *topdir*		"y:/23吉田/未処理/")
(defvar *server-directory*	"f:/util2/kserv/")

(defvar *dock-output-file*	(server-file ".dock"))
(defvar *sc-output-file*	(server-file ".setcontract"))
(defvar *setting-file*		(server-file "setting.lisp"))
(defvar *data3000-output-file*	(server-file ".data3000"))
(defvar *mtime-file*		(server-file "mtime.lisp"))
(defvar *zip-parse-file*	"y:/47伊東/zip-parse.csv")
(defvar *zenken-hash*		"f:/zenken.hash")
(defvar *zenken-file*		(or (cl-fad:file-exists-p #P"d:/特定健診システム/特定健診CSV/特定健診全件データ.csv")
				    (cl-fad:file-exists-p #P"f:/20130628/特定健診全件データ.csv")))
(defvar *zenken-vec-file*	"f:/zenken.vec")
(defvar *csv-output-file*	"f:/util2/csv/temp.csv")
(defvar *csv-except-directory*  '("g:/終了/"))
(defvar *csv-regexp*            "00263129_FKAC522_\\d{8}_\\d{3}")
(defvar *fkca172*		"f:/FKCA172.csv")
(defvar *fkac167*		"f:/FKAC167.csv")
(defvar *fkac165*		"f:/FKAC165.csv")
(defvar *year*			2013)
(defvar *year2*			(mod *year* 1000))
(defvar *usb2*			"g:/")
(defvar *zip-directory*		'("f:/zip/" "d:/zip/"))
(defvar *data3000-file*		"y:/23吉田/特定健診/●事業所健診結果の提供についての同意書及申請書.xlsx")
(defvar *data3000-sheet*	"受付簿")
(defvar *dock-file*		"y:/23吉田/未処理/ドックー全件ファイル.xlsx")
(defvar *dock-init-file1*	"y:/23吉田/未処理/tools/result1.csv")
(defvar *dock-init-file2*	"y:/23吉田/未処理/tools/result2.csv")
(defvar *error-zip-directory*	(cl-fad:pathname-as-directory
				 (merge-pathnames
				  (find-if #'cl-fad:file-exists-p
					   *zip-directory*)
				  "ke26312901")))
(defvar *main-zip-directory*	(find-if
				 #'cl-fad:file-exists-p
				 (mapcar
				  (lambda (p)
				    (cl-fad:pathname-as-directory
				     (merge-pathnames p "MAIN")))
				  *zip-directory*)))
(defvar *hsido-directory*	(cl-fad:pathname-as-directory
				 (merge-pathnames *main-zip-directory* "HSIDO")))
(defvar *hsido-output-file*	(server-file ".hsido"))

;; (symbol-bounding)
;; (symbol-list-bounding)
(in-package :cl-user)
