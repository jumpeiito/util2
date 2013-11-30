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

(defvar *topdir*		"y:/23吉田/未処理/")
(defvar *dock-output-file*	"f:/util2/kserv/.dock")
(defvar *sc-output-file*	"f:/util2/kserv/.setcontract")
(defvar *setting-file*		"f:/util2/kserv/setting.lisp")
(defvar *mtime-file*		"f:/util2/kserv/mtime.lisp")
(defvar *zip-parse-file*	"y:/47伊東/zip-parse.csv")
(defvar *zenken-hash*		"f:/zenken.hash")
(defvar *fkca172*		"f:/FKCA172.csv")
(defvar *fkac167*		"f:/FKAC167.csv")
(defvar *year*			2013)
(defvar *usb2*			"g:/")
(defvar *zip-directory*		'("f:/zip/" "d:/zip/"))
(defvar *dock-file*		"y:/23吉田/未処理/ドックー全件ファイル.xlsx")
(defvar *dock-init-file1*	"y:/23吉田/未処理/tools/result1.csv")
(defvar *dock-init-file2*	"y:/23吉田/未処理/tools/result2.csv")
(defvar *error-zip-directory*	(cl-fad:pathname-as-directory
				 (merge-pathnames
				  (find-if #'cl-fad:file-exists-p
					   *zip-directory*)
				  "ke26312901")))

;; (symbol-bounding)
;; (symbol-list-bounding)
(in-package :cl-user)
