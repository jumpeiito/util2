(defpackage #:meibo
  (:nicknames :mb)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel))

(in-package :mb)

(defvar *year*		ksetting::*year*)
(defvar *nendo-end*	(nendo-end *year*))
(defvar *file*		ksetting::*zenken-file*)
(defvar *zenken-csv*	#P"f:/zenken.csv")
(defvar *zenken-hash*	#P"f:/zenken.hash")

;; (defun %to-zenken ()
;;   (iter (with bhash = (kensin:bunkai-hash))
;; 	(for line :in-csv *file* :code :SJIS)
;; 	(for z = (zenken::create-zenken line bhash))
;; 	(if (and (not (first-time-p))
;; 		 (zenken::target? z ))
;; 	    (collect z))))

;; (defun make-zenken-store ()
;;   (cl-store:store (%to-zenken) *zenken-csv*))

;; (defun zenken-store ()
;;   (unless (cl-fad:file-exists-p *zenken-csv*)
;;     (make-zenken-store))
;;   (cl-store:restore *zenken-csv*))

;; (defun number-for-sort (line)
;;   (optima:match line
;;     ((LIST* jnum _ _ _ _ _ _ _ _ _ _ _ telnum _)
;;      ;; (format nil "~A~A" telnum jnum)
;;      telnum)))

;; (defun kill-blank (string)
;;   (ppcre:regex-replace-all "[　 ]{2,}" string ""))

;; (defun kill-blank-literally (string)
;;   (ppcre:regex-replace-all "[　 ]+" string ""))

;; (defun line-for-print (line bk furi-hash)
;;   (optima:match line
;;     ((LIST* _ name furi gender birthday year k b hk ad pos tel _)
;;      (format nil "~{~A~^,~}"
;; 	     (list k b bk name
;; 		   ;; (if (equal "" (kill-blank furi)))
;; 		   (let ((f (kill-blank furi)))
;; 		     (if (equal "" f)
;; 			 (gethash (format nil "~A~A" k b) furi-hash)
;; 			 f))
;; 		   hk gender birthday year ;; pos
;; 		   (kill-blank-literally ad)
;; 		   tel)))))

;; (defun yet? (zenken)
;;   (with-slots (zenken::受診日 zenken::発行日) zenken
;;     (and (not zenken::受診日) (not zenken::発行日))))

;; (defun %csv-sort (list)
;;   (sort2 list string< number-for-sort))

;; (defun on-csv? (zenken-obj)
;;   (and zenken-obj (zenken::target? zenken-obj) (yet? zenken-obj)))

;; (defun %csv-init (filename)
;;   (csv-read-to-list filename :code :SJIS))

;; (defun make-furigana-hash (list)
;;   (iter (with hash = (make-hash-table :test #'equal))
;; 	(for line :in list)
;; 	(optima:match line
;; 	  ((LIST* _ _ furi _ _ _ k b hk _)
;; 	   (if (equal hk "本人")
;; 	       (setf (gethash (format nil "~A~A" k b) hash)
;; 		     (car (ppcre:split " " furi))))))
;; 	(finally (return hash))))

;; (defun %csv (filename)
;;   (let ((title nil)
;; 	(init  (%csv-init filename)))
;;     (iter (with zhash = (cl-store:restore *zenken-hash*))
;; 	  (with fhash = (make-furigana-hash init))
;; 	  (for line :in init)
;; 	  (if (first-time-p)
;; 	      (progn
;; 		(setq title line)
;; 		(next-iteration)))
;; 	  (optima:match line
;; 	    ((LIST* jnum _)
;; 	     (for obj = (gethash jnum zhash))
;; 	     (if (on-csv? obj)
;; 		 (collect (cons (zenken::zenken-分会名 obj) line)
;; 		   :into pot))))
;; 	  (finally (return (append (list (line-for-print title "分会名" fhash))
;; 				   (mapcar (lambda (c)
;; 					     (line-for-print (cdr c) (car c) fhash))
;; 					   (%csv-sort pot))))))))

;; (defstruct SHIBU pathname code name csv title mainhash)

;; (defun create-shibu (pathname)
;;   (let1 obj (make-shibu :pathname pathname)
;;     (with-slots (pathname code name csv title mainhash) obj
;;       (let1 %csv% (csv-read-to-list pathname :code :SJIS)
;; 	(setq csv	(cdr %csv%)	
;; 	      title	(car %csv%)
;; 	      ))
;;       obj)))

;; (defun %csv2 (filename)
;;   )

;; ;; 特定健診用氏名一覧16東_20131127.csv
;; (defun filename-parse (filename)
;;   (let ((f (pathname-name
;; 	    (make-pathname :defaults filename))))
;;     (cl-ppcre:register-groups-bind
;;     	(code shibu)
;;     	("特定健診用氏名一覧(\\d{2})(.)_\\d{8}.csv" f)
;;       (values (read-from-string code)
;;     	      shibu))))

;; (defun %csv-newname (filename)
;;   (multiple-value-bind (code shibu)
;;       (filename-parse filename)
;;     (make-pathname :defaults filename
;; 		   :name (format nil "電話名簿~A~A" code shibu))))

;; (defun %csv-output (filename)
;;   (call-with-output-file2 (%csv-newname filename)
;;     (lambda (op)
;;       (iter (for line :in (%csv filename))
;; 	    (write-line line op)))
;;     :code :SJIS))

;; ;; #(10 6 9.5 12.5 12.5 12.5 5 5 11 6 12)

;; (defparameter xls-width #(10 6 9.5 12.5 12.5 5 5 11 6 32 13))

;; (defun %xls-width (sh)
;;   (excel::set-colwidth sh xls-width))

;; (defun %xls-align (sh)
;;   (loop
;;      :for row
;;      :in '(:a :b :c :f :g :i)
;;      :do  (setf (slot-value (ole sh :range (format nil "~A:~:*~A" row))
;; 			    :horizontalAlignment)
;; 		excel::xlcenter)))

;; (defun row-topline-erase (sheet row)
;;   (setf (slot-value (ole sheet :range (format nil "A~A:K~:*~A" row) :borders 8)
;; 			    :linestyle)
;; 		nil))

;; ;; (iter (generating line :in x)
;; ;;       (if (equal (last1 line) (last1 (next line)))
;; ;; 	  (print line)))

;; (defun topline-erase (sh uv)
;;   (iter (generating line :in uv)
;; 	(for row :upfrom 1)
;; 	(if (equal (last1 line) (last1 (next line)))
;; 	    (row-topline-erase sh row))))

;; (defun %xls (filename)
;;   (%csv-output filename)
;;   (let ((newname (namestring (%csv-newname filename))))
;;     (with-excel (app :visible t :quit nil)
;;       (with-excel-book (app book newname :close nil)
;; 	(let* ((sh (ole book :Worksheets :item 1))
;; 	       (lr (lastrow sh))
;; 	       (uv (value sh (:a 1) (:k lr))))
;; 	  (%xls-width sh)
;; 	  (%xls-align sh)
;; 	  (borders sh :a 1 :k lr)
;; 	  (topline-erase sh uv)
;; 	  (set-alignment sh (:a 1) (:k 1)))))))

(defpackage #:meibo.core
  (:nicknames #:mbc)
  (:use #:cl #:util #:kensin #:iterate #:cl-win32ole #:excel)
  (:import-from #:optima #:match))

(in-package #:mbc)

(defstruct SHIBU pathname code name csv title clojure)

(defstruct (LINE
	     (:constructor line (jnum name furigana gender birthday year
				      k b hk ad postal telnum knumber ig1 id ig2)))
  jnum name furigana gender birthday year k b hk ad
  postal telnum knumber ig1 id ig2 zenken hnum)

(defun kill-blank-literally (string)
  (ppcre:regex-replace-all "[　 ]+" string ""))

(defun make-line (list zhash)
  (let1 obj (apply #'line list)
    (with-slots (ad jnum zenken hnum) obj
      (setq ad		(kill-blank-literally ad)
	    zenken	(gethash jnum zhash)
	    hnum	(read-from-string
			 (zenken::zenken-保険証番号 zenken))))
    obj))

(defun line-bunkai (line)
  (zenken::zenken-分会名 (line-zenken line)))

(defun line-bunkai-code (line)
  (zenken::zenken-分会 (line-zenken line)))

(defun filename-parse (filename)
  (let ((f (pathname-name
	    (make-pathname :defaults filename))))
    (cl-ppcre:register-groups-bind
    	(code shibu)
    	("特定健診用氏名一覧(\\d{2})(.)_\\d{8}" f)
      (values (read-from-string code)
    	      shibu))))

(defun clojure-sort-key (line)
  (with-slots (zenken) line
    (with-slots (zenken::保険証番号 zenken::行) zenken
      (format nil "~A~A" zenken::保険証番号 zenken::行))))

(defun list-output-sort-key (list)
  (optima:match list
    ((LIST* _ head _)
     (clojure-sort-key head))))

(defun make-shibu-reader-list (hash)
  (sort (alexandria:hash-table-alist hash)
	(lambda (x y)
	  (string< (list-output-sort-key x)
		   (list-output-sort-key y)))))

(def-clojure shibu-reader ()
  ((hash	(make-hash-table :test #'equal)))
  (:add		(line)
		(let1 keyword (line-telnum line)
		  (setf (gethash keyword hash)
			(aif (gethash keyword hash)
			     (sort (copy-list (cons line it))
				   (lambda (x y)
				     (string< (clojure-sort-key x)
					      (clojure-sort-key y))))
			     (list line)))))
  (:list	()
		(iter (for (telnum . data) :in (make-shibu-reader-list hash))
		      (appending data))))

(defun target? (obj)
  (and obj (zenken::target? obj)
       (not (zenken::zenken-受診日 obj))
       (not (zenken::zenken-発行日 obj))))

(defun make-furigana-hash (list)
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in list)
	(optima:match line
	  ((LIST* _ _ furi _ _ _ k b hk _)
	   (if (equal hk "本人")
	       (setf (gethash (format nil "~A~A" k b) hash)
		     (car (ppcre:split " " furi))))))
	(finally (return hash))))

(defun furigana (line hash)
  (optima:match line
    ((LIST* _ _ furi _ _ _ k b _)
     (if (ppcre:scan "^ +$" furi)
  	 (setf (nth 2 line)
  	       (gethash (format nil "~A~A" k b) hash)))
     line)))

(defun %toCSV (list)
  (iter (with hash = (cl-store:restore #P"f:/zenken.hash"))
	(with furihash = (make-furigana-hash list))
	(for line :in list)
	(for jnum  = (car line))
	(for obj   = (gethash jnum hash))
	(if (target? obj)
	    (collect (make-line (furigana line furihash)
				hash)))))

(defun create-shibu-function (csv)
  (let1 f (shibu-reader)
    (dolist (line csv) (funcall f :add line))
    f))

(defun create-shibu (pathname)
  (let1 obj (make-shibu :pathname pathname)
    (let1 init (csv-read-to-list pathname :code :SJIS)
      (multiple-value-bind (vcode vname)
	  (filename-parse pathname)
	(with-slots (pathname code name csv title clojure) obj
	  (setq code	vcode
		name	vname
		csv	(%toCSV (cdr init))
		title	(car init)
		clojure	(create-shibu-function csv)))))
    obj))

(defgeneric expose (s))

(defmethod expose ((line LINE))
  (with-slots (name furigana gender birthday year k b hk ad telnum) line
    (list (line-bunkai line)
	  k b name furigana gender hk birthday year ad telnum)))

(defmethod expose ((shibu SHIBU))
  (with-slots (clojure) shibu
    (iter (for line :in (funcall clojure :list))
	  (collect (expose line)))))

(defparameter width #(9 10 6 15 13 5 5 11 5.5 30 13 25))

(defun %PutData (sheet thread)
  (decide-range-value
   sheet `(("分会" "記号" "番号" "氏名" "フリガナ" "性別" "区分"
		   "生年月日" "年齢" "住所" "電話" "備考・対応")))
  (decide-range-value
   sheet (sb-thread:join-thread thread)
   :start-row 2))

(defun xls (pathname)
  (let1 %thread% (sb-thread:make-thread
  		  (lambda () (expose (create-shibu pathname))))
    (with-excel (app :visible t :quit nil)
      (let* ((book (ole app :Workbooks :Add))
  	     (sh   (ole book :Worksheets :item 1))
	     (lr   (lastrow sh)))
	(%PutData sh %thread%)
	(excel::set-colwidth sh width)
	(border sh (:a 1) (:l lr))))))
