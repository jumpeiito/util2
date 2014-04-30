;; -*- coding:utf-8 -*-
(in-package :kzg)

(defvar topdir #P"d:/zip/ke26312901/")

(defun files-in-zipfile (zipfile)
  (zip:with-zipfile (z zipfile :external-format :utf-8)
    (alexandria:hash-table-keys
     (zip:zipfile-entries z))))

(defun target-zip? (zipfile file-regexp)
  (let ((sb-alien::*default-c-string-external-format* :SJIS))
    (member file-regexp;"FKCA172"
	    (files-in-zipfile zipfile)
	    :test #'ppcre:scan)))

(defun has-file? (directory file-regexp)
  (iter (for file :in-allf directory :type "zip" :regexp "^[0-9a-zA-Z_]+$")
	(if (target-zip? file file-regexp)
	    (collect file))))

;; (defun comma-reader (string &key to)
;;   (declare (optimize speed) (type simple-string string))
;;   (with-input-from-string (i string)
;;     (macrolet ((_read () `(read-char i nil nil nil))
;; 	       (put   () `(cons (get-output-stream-string o) r)))
;;       (labels ((inner (counter c o r)
;; 		 (declare (type fixnum counter))
;; 		 (optima:match c
;; 		   ((TYPE NULL) (reverse (put)))
;; 		   (#\, (inner (1+ counter) (_read) (make-string-output-stream) (put)))
;; 		   (_
;; 		    (if (and to (eq counter to))
;; 			(reverse r)
;; 			(progn
;; 			  (write-char c o)
;; 			  (inner counter (_read) o r)))))))
;; 	(inner 0
;; 	       (_read)
;; 	       (make-string-output-stream)
;; 	       nil)))))

;; (defun make-get-header-line (zipfile-entry num)
;;   (last1 (list-from-string (entry-to-contents zipfile-entry) num)))

;; (defun get-167-csv-header-line (zipfile-entry)
;;   (make-get-header-line zipfile-entry 3))

;; (defun get-172-csv-header-line (zipfile-entry)
;;   (make-get-header-line zipfile-entry 28))

;; (defun get-header-line (num)
;;   (lambda (zipfile-entry)
;;     (make-get-header-line zipfile-entry num)))

;; (defun make-get-jusinken (zipfile-entry func num)
;;   (last1 (comma-reader (funcall func zipfile-entry)
;; 		       :to num)))

;; (defun get-167-csv-jusinken (zipfile-entry)
;;   (make-get-jusinken zipfile-entry
;; 		     (get-header-line 3)
;; 		     10))

;; (defun get-172-csv-jusinken (zipfile-entry)
;;   (make-get-jusinken zipfile-entry
;; 		     (get-header-line 28)
;; 		     12))

;; (defun get-csv-jusinken (header-line-nth to)
;;   (lambda (zipfile-entry)
;;     (make-get-jusinken zipfile-entry
;; 		       (get-header-line header-line-nth)
;; 		       to)))

;; (defun make-get-year (zipfile-entry func)
;;   (+ 2000 (read-from-string (string-take (funcall func zipfile-entry) 2))))

;; (defun get-167-csv-year (zipfile-entry)
;;   (make-get-year zipfile-entry
;; 		 (get-csv-jusinken 3 10)))

;; (defun get-172-csv-year (zipfile-entry)
;;   (make-get-year zipfile-entry
;; 		 (get-csv-jusinken 28 12)))

(defun entry-to-contents (entry)
  (with-decoding-error
      ((sb-ext:octets-to-string
       (zip:zipfile-entry-contents entry)
       :external-format :SJIS))
    ((sb-ext:octets-to-string
       (zip:zipfile-entry-contents entry)
       :external-format :UTF8))))

;; (defun get-contents (zip-instance regexp)
;;   (iter (for (name entry) :in-hashtable (zip:zipfile-entries zip-instance))
;; 	(if (ppcre:scan regexp name)
;; 	    (leave (entry-to-contents entry))
;; 	    (next-iteration))))

(defun get-nth-column (zip-entry nth)
  (mapcar (lambda (line) (nth nth line))
	  (with-input-from-string
	      (i (entry-to-contents zip-entry))
	    (csv-read i))))

(defun get-csv-year (nth)
  (lambda (zip-entry)
    (+ 2000
       (read-from-string
	(most (lambda (str) (if (and str (>= (length str) 2))
				(string-take str 2)
				0))
	      (get-nth-column zip-entry nth))))))

;; (zip:with-zipfile (z #P"d:/zip/ke26312901/ke26312901_20140415090648.zip")
;;   ;; (make-get-header-line z 3)
;;   ;; (funcall (get-nth-column "FKCA172" 11)
;;   ;; 	   z)
;;   ;; (funcall (get-csv-year "FKCA172" 11) z)
;;   (funcall (get-csv-year "FKCA172" 11) z))

;; #P"d:/zip/ke26312901/ke26312901_20131212085805.zip"
(defun make-filter (hash file-regexp func)
  (find-if
     (lambda (key)
       (and (ppcre:scan file-regexp key)
	    (if (string= file-regexp "FKAC165")
		t
		(eq ksetting::*year* (funcall func (gethash key hash))))))
     (alexandria:hash-table-keys hash)))

;; (defun 167-filter (hash)
;;   (make-filter hash "FKAC167" (get-csv-year 9)))

;; (defun 172-filter (hash)
;;   (make-filter hash "FKCA172" (get-csv-year 11)))

(defun filter-function (hash regexp)
  (string-case regexp
    ("FKAC165" (make-filter hash "FKAC165" (get-csv-year 100)))
    ("FKAC167" (make-filter hash "FKAC167" (get-csv-year 9)))
    ("FKCA172" (make-filter hash "FKCA172" (get-csv-year 11)))))

(defun get-csv-name (zip-instance file-regexp)
  (let ((hash (zip:zipfile-entries zip-instance)))
    (string-case file-regexp
      ("FKAC165" (filter-function hash "FKAC165"))
      ("FKAC167" (filter-function hash "FKAC167"))
      ("FKCA172" (filter-function hash "FKCA172")))))

(defun list-from-string (string n)
  (with-input-from-string (i string)
    (labels ((inner (c r)
	       (if (eq c n)
		   (reverse r)
		   (inner (1+ c) (cons (read-line i) r)))))
      (inner 0 nil))))

(defun get-csv-entry (zip-instance file-regexp)
  (gethash (get-csv-name zip-instance file-regexp)
	   (zip:zipfile-entries zip-instance)))

(defun get-csv-contents (zip-instance file-regexp)
  (entry-to-contents
   (get-csv-entry zip-instance file-regexp)))

(defun zip-to-contents (zipname file-regexp)
  (zip:with-zipfile (z zipname)
    (get-csv-contents z file-regexp)))

(defun seek (directory file-regexp)
  (let1 zipfile (car (sort2 (has-file? directory file-regexp)
			    string> pathname-name))
    (zip-to-contents zipfile file-regexp)))

(defun zip-write (contents pathname)
  (alexandria:write-string-into-file
   contents pathname
   :if-exists		:overwrite
   :if-does-not-exist	:create
   :external-format	:sjis))

(defun drive-f-name (string)
  (make-pathname :defaults #P"f:/"
		 :name string
		 :type "csv"))

(defun drive-d-name (string)
  (make-pathname :defaults #P"d:/特定健診システム/"
		 :name (format nil "~A_~A" string (util::today-8))
		 :type "csv"))

(defun drive-y-name (string)
  (make-pathname :defaults #P"y:/47伊東/"
		 :name string
		 :type "csv"))

(defmacro %write (contents string)
  `(progn (util::stdout "~Aファイルを出力中~%" ,string)
	  (zip-write ,contents (drive-f-name ,string))))

(defmacro %copy-d (string)
  `(cl-fad:copy-file (drive-f-name ,string)
		     (drive-d-name ,string)
		     :overwrite t))

(defmacro %copy-y (string)
  `(cl-fad:copy-file (drive-f-name ,string)
		     (drive-y-name ,string)
		     :overwrite t))

(defun getfile ()
  (delete-file-if-exists #P"f:/FKCA172.csv")
  (kzm::move)
  (let ((172contents (seek topdir "FKCA172"))
	(167contents (seek topdir "FKAC167"))
	(165contents (seek topdir "FKAC165")))
    (%write 172contents "FKCA172")
    (%write 167contents "FKAC167")
    (%write 165contents "FKAC165")
    (when (cl-fad:file-exists-p #P"d:/特定健診システム/")
      (%copy-d "FKCA172")
      (%copy-d "FKAC167")
      (%copy-d "FKAC165"))
    (when (cl-fad:file-exists-p #P"y:/47伊東/")
      (%copy-y "FKCA172")
      (%copy-y "FKAC167")
      (%copy-y "FKAC165"))
    (cerr::newest)))

(in-package :cl-user)
