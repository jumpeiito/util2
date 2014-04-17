(in-package :cerr)

(defparameter %directory
  (find-if #'cl-fad:file-exists-p
	   ksetting::*zip-directory*))

(defstruct format-error-csv
  pathname created occurd hcode hname index body errors)

(defstruct xml
  pathname created occurd hcode hname body errors)

(defstruct xmlline
  pathname kigo bango name birthday gender number error-id
  error-message lineno upload? shibu)

(defun csv-error-file-p (name)
  (ppcre:scan "00263129_FKAC522_\\d{8}_\\d{3}_\\d{8}_ERR" name))

(defun xml-error-file-p (name)
  (ppcre:scan "\\d{10}_00263129_\\d{9}_\\d_FKAC053_ERR" name))

(defun %purify (string)
  (coerce
   (remove-if (lambda (ch) (char-equal ch #\zero_width_no-break_space))
	      (coerce string 'list))
   'string))

(defun error-file-contents (entry)
  (%purify
   (sb-ext:octets-to-string
    (zip:zipfile-entry-contents entry)
    :external-format :SJIS)))

(defmacro with-string-to-csv ((filename entry constructor) &rest body)
  `(let ((string (error-file-contents ,entry)))
     (with-input-from-string (input string)
       (let1 obj (,constructor :pathname (last1 (ppcre:split "/" ,filename)))
	 (let1 init (util:csv-read input)
	   ,@body
	   obj))))) ;; ==> obj init は束縛される

(defun create-csv-by-string (entry name)
  (with-string-to-csv (name entry kcsv::make-csv)
    (kcsv::with-csv-slots obj
      (setq kcsv::hcode		(kcsv::cells init 2 2)
	    kcsv::hname		(car (gethash kcsv::hcode kensin:hospital-hash))
	    kcsv::occurd	(kcsv::cells init 2 4)
	    kcsv::created	(kcsv::cells init 1 4)
	    kcsv::index		(kcsv::index-clojure (second init))
	    kcsv::body		(iter (for line :in init)
				      (for row :upfrom 1)
				      (if (and (equal (car line) "5")
					       (not (equal (last1 line) "MKAC05209E:すでに健診結果データが登録されています。")))
					  (collect (kcsv::create-line line row index))))
	    kcsv::errors	(iter (for line :in kcsv::body)
				      (collect (last1 (kcsv::line-contents line))))))))

(defun create-format-error-csv (entry name)
  (with-string-to-csv (name entry make-format-error-csv)
    (with-slots (hcode hname occurd created index body errors) obj
      (setq hcode	(kcsv::cells init 2 2)
	    hname	(car (gethash hcode kensin:hospital-hash))
	    occurd	(kcsv::cells init 2 4)
	    created	(kcsv::cells init 1 4)
	    index	(kcsv::index-clojure (second init))
	    body	(butlast (cdr init))
	    errors	nil))))

(defun pathname->hcode (pathname)
  (cl-irregsexp::if-match-bind
   ((hcode (string 10)) "_00263129_" (create (string 8)) _)
   (the string pathname)
   (values hcode create)))

(defun pathname->lineno (pathname)
  (ppcre:register-groups-bind (num)
      ("(\\d+)" pathname)
    (mod (read-from-string num)
	 1000)))

(defun shibu (kigo)
  (kensin::long-shibu
   (to-hankaku (string-take-right kigo 2))))

(defun create-xmlline (list 172hash)
  (optima:match list
    ((LIST path _ _ _ mes)
     (make-xmlline :pathname path
		   :error-message mes))
    ((LIST path _ k b name birth sex num _ id mes)
     (make-xmlline :pathname	path
		   :kigo	k
		   :bango	(to-zenkaku b)
		   :name	name
		   :birthday	(normal-date->string (strdt birth))
		   :gender	sex
		   :number	num
		   :error-id	id
		   :error-message mes
		   :lineno	(pathname->lineno path)
		   :upload?	(r172c::172-uploaded? num 172hash)
		   :shibu	(shibu k)))))

(defun create-xml-by-string (entry name 172hash)
  (with-string-to-csv (name entry make-xml)
    (with-slots (pathname created occurd hcode hname body errors) obj
      (multiple-value-bind (hc cr)
	  (pathname->hcode pathname)
	(setq hcode	hc
	      hname	(car (gethash hcode kensin:hospital-hash))
	      occurd	nil
	      created	(normal-date->string (strdt cr))
	      body	(mapcar (lambda (line)
				  (create-xmlline line 172hash))
	      			(butlast (cdr init)))
	      errors    (mapcar #'xmlline-error-message
				body))))))

(defmethod csverrror (arg op hash))
(defmethod csverror ((l XMLLINE) op hash)
  (with-slots (pathname kigo bango name birthday gender
			number error-id error-message lineno
			upload? shibu) l
    (format nil "~{~A~^,~}"
	    (mapcar (lambda (n) (or n ""))
		    (list lineno number shibu bango name birthday
			  "" "" "" error-message upload?)))))

(defmethod csverror ((l KCSV::LINE) op (hash hash-table))
  (with-slots (kcsv::name kcsv::bango kcsv::birthday
			  kcsv::number kcsv::contents kcsv::row
			  kcsv::shibu kcsv::merr kcsv::eerr kcsv::rerr) l
    (format nil "~{~A~^,~}~%"
	    (list kcsv::row kcsv::number (kensin::long-shibu kcsv::shibu)
		  kcsv::bango kcsv::name kcsv::birthday
		  (apply #'+ kcsv::merr)
		  (apply #'+ kcsv::eerr)
		  (apply #'+ kcsv::rerr)
		  (kcsv::cell-contents (last1 kcsv::contents))
		  ;; (kensin::172-uploaded? kcsv::number hash)
		  (r172c::172-uploaded? kcsv::number hash)))))

(defmethod csverror ((c KCSV::CSV) op 172hash)
  (optima:match c
    ((KCSV::CSV kcsv::occurd kcsv::hcode kcsv::hname kcsv::body)
     (iter (for line :in kcsv::body)
	   (format op "~A,~A,~A,~A,~A"
	   	   kcsv::occurd kcsv::hcode kcsv::hname
	   	   (kcsv::csv-pathname c)
	   	   (csverror line op 172hash))))))

(defmethod csverror ((c FORMAT-ERROR-CSV) op 172hash)
  (optima:match c
    ((FORMAT-ERROR-CSV occurd hcode hname body pathname)
     (format op "~A,~A,~A,~A,~A"
	     occurd hcode hname pathname
	     (format nil "~{~A~^,~}"
		     (list "" "" "" "" "" ""
			   "" "" "" (util:cell body (2 4)) ""))))))

(defmethod csverror ((x XML) op hash)
  (declare (ignorable hash))
  (with-slots (pathname created occurd hcode hname body) x
    (iter (for line :in body)
	  (format op "~{~A~^,~}~%"
		  (list occurd hcode hname pathname
			(csverror line nil nil))))))

(defparameter title
  "受診日,医療機関コード,医療機関,ファイル名,行,整理番号,支部,番号,氏名,生年月日,必須,選択,相関,エラー,UPLOAD")

(defun error-file-name (zipfile &key (type "csv"))
  (make-pathname :defaults zipfile
		 :type type))

(defun %error-csv (entry name op hash)
  (handler-case
      ;; 通常のファイル(CSV)
      ;; (csverror (create-csv-by-string entry name) op hash)
      (print (create-csv-by-string entry name))
    (sb-kernel:nil-array-accessed-error (e)
      (declare (ignorable e))
      ;; フォーマット・形式的なエラーが出るもの
      (csverror (create-format-error-csv entry name) op hash))))

(defun zipfile-entries (zipfile)
  (mapcar #'car
	  (zip:with-zipfile (z zipfile)
	    (alexandria:hash-table-alist
	     (zip:zipfile-entries z)))))

;; f:/zip/ke26312901/ke26312901_20131111103136.zip
(defgeneric object-errors (arg))
(defmethod object-errors ((csv KCSV::CSV))
  (kcsv::csv-errors csv))
(defmethod object-errors ((csv FORMAT-ERROR-CSV))
  (format-error-csv-errors csv))
(defmethod object-errors ((xml XML))
  (xml-errors xml))
(defmethod object-errors ((n NULL))
  nil)

(defun has-error-file? (zipfile)
  (find-if (lambda (p)
	     (or (csv-error-file-p p)
		 (xml-error-file-p p)))
	   (zipfile-entries zipfile)))

(defun %test ()
  (directory-list #P"f:/zip/ke26312901/" :type "zip"))

(defun error-file-make-object (hash entry name)
  (optima:match name
    ((satisfies csv-error-file-p)
     (handler-case (create-csv-by-string entry name)
       (sb-kernel:nil-array-accessed-error (e)
	 (declare (ignore e))
	 (create-format-error-csv entry name))))
    ((satisfies xml-error-file-p)
     (create-xml-by-string entry name hash))))

(defun error-file-collect (zipfile hash)
  (zip:with-zipfile (z zipfile)
    (iter (for (name entry)
	    :in-hashtable (zip:zipfile-entries z))
	  (for obj = (error-file-make-object hash entry name))
	  (if (object-errors obj)
	      (collect obj)))))

(defun error-csvfile-writer (zipfile output-file func)
  (call-with-output-file2 (or output-file (error-file-name zipfile))
    (lambda (op)
      (format op "~A~%" title)
      (funcall func op))
    :code :SJIS))

(defun error-file (zipfile &key (output-file nil))
  ;; (let ((hash (kensin::172-hash)))
  (let ((hash (r172c::172-hash)))
    (if (has-error-file? zipfile)
	(aif (error-file-collect zipfile hash)
	     (error-csvfile-writer
	      zipfile
	      output-file
	      (lambda (op)
		(iter (for line :in it)
		      (csverror line op hash))))
	     (format nil "~A エラーはありません。~%" zipfile))
	(format nil "~A エラーファイルはありません。~%" zipfile))))

(defun %newest-file ()
  (car
   (sort2 (directory-list ksetting::*error-zip-directory*
			  :type   "zip"
			  :regexp "ke26312901_\\d{14}")
	  string> namestring)))

(defun newest ()
  (let ((file (%newest-file)))
    (aif (error-file file)
	 (format t "~A" it)
	 (with-excel (app :quit nil)
	   (with-excel-book (app _ (namestring
				    (make-pathname :defaults file :type "csv"))
				 :close nil))))))

(defparameter xls-width (vector 10 11 15 0 4 12 8.5 6 16 10 5 5 5 30 10))

(defun %how-old (jnumber birth)
  (if-match-bind
      ((year (integer :length 2)) _)
      (the string (format nil "~A" (truncate jnumber)))
    (util:how-old (util::to-string birth)
		  (nendo-end (+ 2000 year)))))

(defun error-file-xls (zipfile)
  (with-excel (app :visible t :quit nil)
    (with-excel-book (app book (namestring (error-file-name zipfile))
			  :close nil)
      (let* ((sh (ole book :worksheets :item 1))
	     (lr (lastrow sh :y 1 :x 1)))
	(border sh (:a 1) (:o lr))
	(set-alignment sh 1 :horizontalAlignment excel::xlcenter)
	(iter (for col :from 1 :to (length xls-width))
	      (set-width sh (format nil "~A" (aref util::alph (1- col)))
			 (svref xls-width (1- col))))

	(iter (for row :from 2 :to lr)
	      (for val = (value sh (:o row)))
	      (for num = (value sh (:f row)))
	      (for year = (%how-old num (value sh (:j row))))
	      (cond
		((or (equal "UPLOADED" val)
		     (> year 74)
		     (< year 40))
		 (set-colorindex sh (:a row) (:o row) :interior excel::xlgray50))
		((ppcre:scan "(取得|喪失)" val)
		 (set-colorindex sh (:a row) (:o row) :interior excel::xlblack)
		 (set-colorindex sh (:a row) (:o row) :font excel::xlwhite))))

	(excel::save-book book zipfile :xlsx)))))

(in-package :cl-user)
