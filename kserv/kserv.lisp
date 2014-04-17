(in-package :kserv)

;; (defparameter topdir #P"y:/23吉田/未処理/")
;; (defparameter targetfile #P"y:/23吉田/未処理/解析対象.txt")
;; (defvar *dock-output-file* "f:/util2/kserv/.dock")
;; (defvar *setting-file* "f:/util2/kserv/setting.lisp")
;; (defvar *mtime-file* "f:/util2/kserv/mtime.lisp")

(defun server-collect ()
  ;; (util:allf ksetting::*topdir*
  ;; 	     :type "zip" :regexp "^\\d{10}_00263129_\\d{9}_[0-9]$")
  (unless (file-exists-p ksetting::*topdir*)
    (error "kserv::server-collect-error"))
  (util:directory-list ksetting::*topdir*
		       :type "zip" :regexp "^\\d{10}_00263129_\\d{9}_[0-9]$"))

(defun zip-directory-build ()
  (optima:match ksetting::*zip-directory*
    ((LIST d1 d2)
     (let ((dmain1 (pathname-as-directory
		    (merge-pathnames d1 "MAIN")))
	   (dmain2 (pathname-as-directory
		    (merge-pathnames d2 "MAIN"))))
       (kzm::directory-compare d1 d2)
       (kzm::directory-compare d2 d1)
       (kzm::directory-classify d1)
       (handler-case (kzm::main-build dmain1)
	 (simple-error (e) (declare (ignorable e))
		       (print e)))
       (kzm::directory-classify d2)
       (handler-case (kzm::main-build dmain2)
	 (simple-error (e) (declare (ignorable e))
		       (print e)))))))

(defun dock-parse-target-files ()
  (with-open-file (i targetfile :direction :input)
    (loop
       :for line = (read-line i nil nil nil)
       :while line
       :collect (ppcre:regex-replace-all "" line ""))))

#|
予約日		2
院所		3
記号番号	5
氏名		6
生年月日	7
整理番号	8
半日		17
脳		18
肺		19
ペット		20
受診日		21
費用		22
支払日		23
支払月		24
|#
(defun make-dock-parse-excel-file ()
  (with-excel (app :visible nil :quit t)
    (iter (for file :in (dock-parse-target-files))
	  (appending
	   (with-excel-book (app book file :close t)
	     (let* ((sh (ole book :worksheets :item "リスト")))
	       (iter (for row :from 4 :to (lastrow sh :y 3 :x 12))
		     (print row)
		     (if (slot-value (cl-win32ole:ole sh :cells row 2) 'value)
			 (collect (append (excel:ole-value sh :range (format nil "B~A:H~A" row row))
					  (excel:ole-value sh :range (format nil "Q~A:X~A" row row))))))))))))

(defun truncate-or-nil (k)
  (if k (truncate k) ""))

(defun setting ()
  (call-with-input-file2
      ksetting::*setting-file* #'read))

(defun setting-files ()
  (if (file-exists-p ksetting::*setting-file*)
      (reduce (lambda (x y)
		(let ((f (second y)))
		  (optima:match f
		    ((type list) (append x f))
		    ((type atom) (cons f x)))))
	      (group (setting) 2)
	      :initial-value nil)))

(defun create-mtime-hash ()
  (call-with-output-file2 ksetting::*mtime-file*
    (lambda (op)
      (iter (for file :in (setting-files))
  	    (appending (list file (util::mtime file)) :into pot)
  	    (finally (write pot :stream op))))))

(defun mtime-hash ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for (file mtime)
	  :in (group (call-with-input-file2
			 ksetting::*mtime-file* #'read) 2))
	(sethash file mtime hash)
	(finally (return hash))))

(defun setcontract-file ()
  (getf (setting) :setcontract))

(defun uploaded? (file)
  (multiple-value-bind (val hit)
      (gethash file (mtime-hash))
    (declare (ignore hit))
    (not (equal (util::mtime file) val))))

(defun excel-line->lisp-list (line)
  (mapcar (lambda (el)
	    (typecase el
	      (dt:date-time (util::to-string el))
	      (float        (truncate el))
	      (null         "")
	      (t            el)))
	  line))

(defun make-setcontract-csv ()
  (if (file-exists-p #P"d:/特定健診システム/特定健診集合契約データ/")
      (call-with-output-file2
	  #P"d:/特定健診システム/特定健診集合契約データ/read.csv"
	  (lambda (op)
	    (format op "~{~A,,~%~}"
		    (mapcar (lambda (line)
			      (let ((car (car line)))
				(if (eq 11 (length car)) car (format nil "0~A" car))))
			    (csv-read-to-list ksetting::*sc-output-file*)))))))

(defun setcontract-reader (&key (force nil))
  (let1 file (setcontract-file)
    (if (or (uploaded? file) force)
	(progn
	  (format t "集合契約ファイルを解析します。")
	  (with-excel (app :visible t :quit t)
	    (with-excel-book (app book file :close t)
	      (with-open-file (o ksetting::*sc-output-file*
				 :if-exists :supersede
				 :direction :output)
		(let1 ur (ole book :worksheets :item "リスト" :usedrange :value)
		  (mapcar (lambda (line) (format o "~{~A~^,~}~%" (excel-line->lisp-list line)))
			  (cdr ur))))))
	  (make-setcontract-csv)
	  (format t "集合契約ファイルを解析しました。")))))

(defun dock-file ()
  (getf (setting) :dock))

(defun dock-reader-not-blank-line? (line)
  (destructuring-bind (l1 l2 . rest) line
    (declare (ignorable rest))
    (or (and l1 (typep l1 'dt:date-time))
	(and l2 (typep l2 'dt:date-time)))))

(defun take-nth (line list)
  (mapcar (lambda (n) (nth n line))
	  list))

(defun dock-reader-book-check-error (bk)
  (let* ((sh (ole bk :Worksheets :item "リスト"))
	 (lr (lastrow sh :y 3 :x 6)))
    (iter (for i :from 4 :to lr)
	  (handler-case (value sh (:a i) (:ae i))
	    (simple-error (e)
	      (declare (ignorable e))
	      (format t "~A行目" i))))))

(defun dock-reader-book (excelapp filename)
  (with-excel-book (excelapp book filename :close t)
    (handler-case
	(iter (for line :in (ole book :worksheets :item "リスト" :usedrange :value))
	      (for n :upfrom 4)
	      (when (dock-reader-not-blank-line? line)
		(for xline = (take-nth line '(1 2 4 5 6 7 16 17 18 19 20 21 22 23)))
		(collect (excel-line->lisp-list xline))))
      (simple-error (e)
	(declare (ignorable e))
	(format t "~Aでエラーがあるようです。" filename)
	(dock-reader-book-check-error book)))))

(defun dock-reader ()
  (format t "ドック支払ファイルを解析します。~%")
  (with-excel (app :visible nil :quit t)
    (iter (for file :in (dock-file))
	  (format t "~A~%" file)
	  (appending (dock-reader-book app file) :into pot)
	  (finally (progn
		     (format t "ドック支払ファイルを解析しました。~%")
		     (return pot))))))

;; (defun multi-thread-dock-reader ()
;;   (format t "ドック支払ファイルを解析します。~%")
;;   (iter (for file :in (dock-file))
;; 	(collect (sb-thread:make-thread
;; 		  (lambda ()
;; 		    (print file)
;; 		    (with-excel (app :visible nil :quit t)
;; 		      (dock-reader-book app file))))
;; 	  :into pot)
;; 	(finally (prog1
;; 		     (return (mapcan #'sb-thread:join-thread pot))
;; 		   (format t "ドック支払ファイルを解析しました。~%")))))

(defun parse-dock-reader (rd)
  (mapcar (lambda (line)
	    (format nil "~{~A~^,~}~%"
		    (mapcar (lambda (n) (nth n line))
			    (list 5 6 10))))
	  rd))

(defun dock-writer (&key (force nil))
  (when (or force (any #'uploaded? (dock-file)))
    (let ((read-data (dock-reader))
	  (output #P"d:/特定健診システム/特定健診人間ドックデータ/read.csv")
	  (odir #P"d:/特定健診システム/特定健診人間ドックデータ/"))
      (call-with-output-file2 ksetting::*dock-output-file*
	(lambda (op)
	  (iter (for line :in read-data)
		(format op "~{~A~^,~}~%" line))))
      (when (file-exists-p odir)
	(call-with-output-file2
	    output
	    (lambda (op)
	      (format op "~{~A~}" (parse-dock-reader read-data))))
	(format t "K接続読み込み用ファイルを作成しました。~% -> ~A" output)))))

(defun make-parse-file ()
  (util::stdout "zip解析ファイルを作ります。~%")
  (call-with-output-file2 "y:/47伊東/zip-parse.csv"
    (lambda (op)
      ;; (iter (for line :in (dock::parse))
      (iter (for line :in (dock::%parse 10))
	    (format op "~{~A~^,~}~%" line)))
    :code :SJIS)
  (util::stdout "zip解析ファイルを作りました。~%"))

(defun make-zenken-hash-store (&key (force nil))
  (let1 file (getf (setting) :zenken)
    (when (or (uploaded? file) force)
      (util::stdout "全件データのハッシュ作成開始~%")
      (if (file-exists-p #P"d:/特定健診システム/") ;add +3
	  (copy-file #P"d:/特定健診システム/特定健診CSV/特定健診全件データ.csv"
		     #P"f:/20130628/特定健診全件データ.csv"
		     :overwrite t))
      (cl-store:store (zenken::make-jusinken-hash file)
		      ksetting::*zenken-hash*)
      (zenken:vec-output)
      (util::stdout "全件データのハッシュ作成終了~%"))))

(defun make-zenken-array-store ()
  (let1 file (getf (setting) :zenken)
    (when (uploaded? file)
      (util::stdout "全件データのハッシュ作成開始~%")
      (cl-store:store (zenken::make-jusinken-hash file) "f:/zenken.array")
      (util::stdout "全件データのハッシュ作成終了~%"))))

(defun zipfile-move ()
  (aif (server-collect)
       (progn
	 (let ((ehash (kzm::exist-hash #P"d:/zip/")))
	   (iter (for zip :in it)
		 (if (gethash (pathname-name zip) ehash)
		     (rename-file zip
				  (make-pathname :defaults zip
						 :name (format nil "(重複)~A" (pathname-name zip))))
		     (rename-file zip (merge-pathnames #P"d:/zip/" zip))))
	   (zip-directory-build)
	   (make-parse-file)
	   (hsido:@write)))))

(defmacro with-data3000-output ((output-port) &rest form)
  `(call-with-output-file2 ,ksetting::*data3000-output-file*
     (lambda (,output-port) ,@form)))

(defmacro with-data3000-sheet ((sheet-name) &rest form)
  `(with-excel (app :visible nil :quit t)
     (with-excel-book (app book ,ksetting::*data3000-file* :close t)
       (let* ((,sheet-name (ole book :WorkSheets :Item ,ksetting::*data3000-sheet*)))
	 ,@form))))

(defun data3000-writer (&key (force nil))
  (if (or force (uploaded? ksetting::*data3000-file*))
      (let ((hash (r172c::172-hash)))
	(with-data3000-output (op)
	  (with-data3000-sheet (sh)
	    (iter (for line :in (ole sh :usedrange :value))
		  (optima:match line
		    ((LIST* _ nil _) (next-iteration))
		    ((LIST* _ "受診日" _) (next-iteration))
		    ((LIST* _ date kgbg _ name jnumber hname hcode _)
		     (format op "~{~A~^,~}~%"
			     (excel::excel-line->lisp
			      (list date kgbg jnumber name hname hcode
				    (if (gethash (& write-to-string truncate jnumber) hash)
					"○" "×"))))))))))))

(defmacro ->second (d)
  `(+ (* 60 60 (dt:hour-of ,d))
      (* 60    (dt:minute-of ,d))
      (dt:second-of ,d)
      (float (/ (dt:millisecond-of ,d) 1000))))

(defmacro millisecond- (d1 d2)
  `(- (->second ,d1) (->second ,d2)))

(defmacro timer (&rest form)
  `(let ((begin (dt:now)))
     (values
      (progn ,@form)
      (let ((end (dt:now)))
	(millisecond- end begin)))))

(defparameter benchmark-verbose nil)

(defmacro @benchmark (&rest args)
  `(let ((func-name (car args)))
     (multiple-value-bind (result second)
	 (timer ,args)
       (prog1
	   result
	 (if benchmark-verbose
	     (format t "~A秒(~A)~%" second func-name)
	     nil)))))

(defun watch-execute ()
  ;; (@benchmark zipfile-move)
  ;; (@benchmark setcontract-reader)
  ;; (@benchmark dock-writer)
  ;; (@benchmark make-zenken-hash-store)
  ;; (@benchmark data3000-writer)
  (zipfile-move)
  (setcontract-reader)
  (dock-writer)
  (make-zenken-hash-store)
  (create-mtime-hash))

(defun watch-execute-force ()
  (setcontract-reader     :force t)
  (dock-writer            :force t)
  (make-zenken-hash-store :force t)
  ;; (data3000-writer        :force t)
  (create-mtime-hash)
  (make-parse-file))

(defun to-string (dt)
  (format nil "~A/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	  (dt:year-of dt) (dt:month-of dt) (dt:day-of dt)
	  (dt:hour-of dt) (dt:minute-of dt) (dt:second-of dt)))

(defun watch ()
  (loop
     ;; (aif (server-collect) (watch-execute it) nil)
     (format t "~A  起動~%" (to-string (dt:now)))
     (watch-execute)
     (format t "~A  終了~%" (to-string (dt:now)))
     (sleep 120)))

(defun start ()
  (bt::%make-thread #'watch "ysdWatcher"))

(defun this-thread ()
  (find-if (lambda (th) (string= "ysdWatcher"
				 (bt:thread-name th)))
	   (bt:all-threads)))

(defun stop ()
  (bt:destroy-thread (this-thread)))


(in-package :cl-user)
