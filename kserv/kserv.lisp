(in-package :kserv)

;; (defparameter topdir #P"y:/23吉田/未処理/")
;; (defparameter targetfile #P"y:/23吉田/未処理/解析対象.txt")
;; (defvar *dock-output-file* "f:/util2/kserv/.dock")
;; (defvar *setting-file* "f:/util2/kserv/setting.lisp")
;; (defvar *mtime-file* "f:/util2/kserv/mtime.lisp")

(defun server-collect ()
  (util:allf ksetting::*topdir*
	     :type "zip" :regexp "^\\d{10}_00263129_\\d{9}_[0-9]$"))

(defun zip-directory-build ()
  (optima:match ksetting::*zip-directory*
    ((LIST d1 d2)
     (let ((dmain1 (cl-fad:pathname-as-directory
		    (merge-pathnames d1 "MAIN")))
	   (dmain2 (cl-fad:pathname-as-directory
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
		     (if (slot-value (cl-win32ole:ole sh :cells row 2) 'value)
			 (collect (append (excel:ole-value sh :range (format nil "B~A:H~A" row row))
					  (excel:ole-value sh :range (format nil "Q~A:X~A" row row))))))))))))

(defun truncate-or-nil (k)
  (if k (truncate k) ""))

;; (defun make-dock-parse ()
;;   (with-open-file (op "y:/23吉田/未処理/tools/ysddata"
;; 		      :direction :output
;; 		      :external-format :UTF-8
;; 		      :if-exists :supersede
;; 		      :if-does-not-exist :create)
;;     (format t "ドック請求ファイルを解析します。~%")
;;     (iter (for line :in (make-dock-parse-excel-file))
;; 	  (optima:match line
;; 	    ((list date1 hp shibu kgbg name birth number k brain k2 k3 date2 pay date3 date4)
;; 	     (format op "~{~A~^,~}~%"
;; 		     (list (util::to-string date1)
;; 			   hp (truncate-or-nil kgbg) name
;; 			   (util::to-string birth)
;; 			   (truncate-or-nil number)
;; 			   (truncate-or-nil k)
;; 			   (truncate-or-nil brain)
;; 			   (truncate-or-nil k2)
;; 			   (truncate-or-nil k3)
;; 			   (util::to-string date2)
;; 			   (truncate-or-nil pay)
;; 			   (util::to-string date3)
;; 			   date4)))))
;;         (format t "ドック請求ファイルの解析が終了しました。~%")))
(defun setting ()
  (call-with-input-file2
      ksetting::*setting-file* #'read))

(defun setting-files ()
  (if (cl-fad:file-exists-p ksetting::*setting-file*)
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

(defun dock-reader-book (excelapp filename)
  (with-excel-book (excelapp book filename :close t)
    (iter (for line :in (ole book :worksheets :item "リスト" :usedrange :value))
	  (when (dock-reader-not-blank-line? line)
	    (for xline = (take-nth line '(1 2 4 5 6 7 16 17 18 19 20 21 22 23)))
	    (collect (excel-line->lisp-list xline))))))

(defun dock-reader ()
  (format t "ドック支払ファイルを解析します。~%")
  (with-excel (app :visible nil :quit t)
    (iter (for file :in (dock-file))
	  (format t "~A~%" file)
	  (appending (dock-reader-book app file) :into pot)
	  (finally (progn
		     (format t "ドック支払ファイルを解析しました。~%")
		     (return pot))))))

(defun dock-writer (&key (force nil))
  (if (or force (any #'uploaded? (dock-file)))
      (call-with-output-file2 ksetting::*dock-output-file*
	(lambda (op)
	  (iter (for line :in (dock-reader))
		(format op "~{~A~^,~}~%" line))))))

(defun make-parse-file ()
  (util::stdout "zip解析ファイルを作ります。~%")
  (call-with-output-file2 "y:/47伊東/zip-parse.csv"
    (lambda (op)
      (iter (for line :in (dock::parse))
	    (format op "~{~A~^,~}~%" line)))
    :code :SJIS)
  (util::stdout "zip解析ファイルを作りました。~%"))

(defun make-zenken-hash-store (&key (force nil))
  (let1 file (getf (setting) :zenken)
    (when (or (uploaded? file) force)
      (util::stdout "全件データのハッシュ作成開始~%")
      (cl-store:store (zenken::make-jusinken-hash file)
		      ksetting::*zenken-hash*)
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
	   (make-parse-file)))))

(defun watch-execute ()
  (zipfile-move)
  (setcontract-reader)
  (dock-writer)
  (make-zenken-hash-store)
  (create-mtime-hash)
  (make-parse-file))

(defun watch-execute-force ()
  (setcontract-reader     :force t)
  (dock-writer            :force t)
  (make-zenken-hash-store :force t)
  (create-mtime-hash))

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

;; (excel:with-excel (app :visible nil :quit t :debugger t)
;;   (excel:with-excel-book (app book "f:/util2/kserv/test.xlsx" :close t :debugger t)
;;     (cl-win32ole:ole book :range "A3:A3" :value)))

(in-package :cl-user)
