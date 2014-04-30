;; (put 'cl-fad:walk-directory 'lisp-indent-function 3)
(in-package :kzm)

(defun dest-path (dest path)
  (make-pathname 
   :defaults (merge-pathnames dest (pathname-name path))
   :type "zip"))

(defun exist-hash (directory)
  (format *standard-output* "ハッシュを作成します。~%")
  (let1 h (iter (for zip :in-allf directory :type "zip")
		(chash zip :key #'pathname-name))
    (format *standard-output* "ハッシュを作成しました。~%")
    h))

(defun directory-compare (src-dir dest-dir &key (type :copy))
  ;;; コピー元・コピー先のディレクトリがなければ作動しない。
  (if (and (cl-fad:file-exists-p src-dir)
	   (cl-fad:file-exists-p dest-dir))
      (let1 exhash (exist-hash dest-dir)
	(let1 duplist nil
	  (util::stdout "元: ~A~%" src-dir)
	  (util::stdout "先: ~A~%" dest-dir)
	  ;; 本体
	  (cl-fad:walk-directory
	   src-dir
	   (lambda (path)
	     (if
	      ;; 拡張子が"zip"で、ファイル名が英数字から成っていて、コピー先に同名のファイルがない。
	      ;; コピー先にファイルがあるかどうかの判断は、ディレクトリを除いたファイル名の部分だけで
	      ;; 判断する。
	      (and (equal "zip" (pathname-type path))
		   (cl-ppcre:scan "^[-A-z0-9_]+$" (pathname-name path))
		   (not (gethash (pathname-name path) exhash nil)))
	      (progn
		(format *standard-output* "~A ~%=> ~A~%"
			path (dest-path dest-dir path))
		(handler-case (funcall (case type
					 (:copy   #'cl-fad:copy-file)
					 (:rename #'rename-file)
					 (t       (lambda (p d) (print (list p d)))))
				       path (dest-path dest-dir path))
		  ;; 重複したときに発生するエラー
		  (#+sbcl sb-int:simple-file-error
		   #+clisp system::simple-file-error (e)
		    (setq duplist (cons (list path (dest-path dest-dir path)) duplist))))))))
	  (when duplist
	    (format *standard-output* "以下のファイルが重複しました。~%")
	    (format *standard-output* "~{~A~%~}" duplist))))))

(defun dir/comp
    (src-dir dest-dir exhash-thread &key (type :copy))
  ;;; コピー元・コピー先のディレクトリがなければ作動しない。
  (let1 exhash (join-thread exhash-thread)
    (let1 duplist nil
      (util::stdout "元: ~A~%" src-dir)
      (util::stdout "先: ~A~%" dest-dir)
      ;; 本体
      (cl-fad:walk-directory
       src-dir
       (lambda (path)
	 (if
	  ;; 拡張子が"zip"で、ファイル名が英数字から成っていて、コピー先に同名のファイルがない。
	  ;; コピー先にファイルがあるかどうかの判断は、ディレクトリを除いたファイル名の部分だけで
	  ;; 判断する。
	  (and (equal "zip" (pathname-type path))
	       (cl-ppcre:scan "^[-A-z0-9_]+$" (pathname-name path))
	       (not (gethash (pathname-name path) exhash nil)))
	  (progn
	    (format *standard-output* "~A ~%=> ~A~%"
		    path (dest-path dest-dir path))
	    (handler-case (funcall (case type
				     (:copy   #'cl-fad:copy-file)
				     (:rename #'rename-file)
				     (t       (lambda (p d) (print (list p d)))))
				   path (dest-path dest-dir path))
	      ;; 重複したときに発生するエラー
	      (#+sbcl sb-int:simple-file-error
		#+clisp system::simple-file-error (e)
		(setq duplist (cons (list path (dest-path dest-dir path)) duplist))))))))
      (when duplist
	(format *standard-output* "以下のファイルが重複しました。~%")
	(format *standard-output* "~{~A~%~}" duplist)))))

(defun mkdir (directory)
  (iter (for dir :in (list "FKAC163" "FKAC165" "FKAC167" "FKAC168" "FKCA172" "TKAC" "MAIN"
			   "FKAC522_ERR" "FKAC053_ERR" "FKBD053_ERR" "FKAB351_ERR"
			   "FKBB331_ERR" "ke26312901"))
	(util::make-directory (cl-fad:pathname-as-directory
			 (merge-pathnames directory dir)))))

(defun copy (pathname dest-parent dest)
  (let1 destination (merge-pathnames
		     (cl-fad:pathname-as-directory
		      (merge-pathnames dest-parent dest))
		     pathname)
    (util::stdout "~A => \"~A\"に移動~%" pathname dest)
    (rename-file pathname destination)))

(defun directory-classify (directory)
  (mkdir directory)
  (iter (for zip :in-directory directory :type "zip")
	(let1 name (pathname-name zip)
	  (rxmatch-case name
			("FKAB351.+ERR" () (copy zip directory "FKAB351_ERR"))
			("TKAC"         () (copy zip directory "TKAC"))
			("FKAC163"      () (copy zip directory "FKAC163"))
			("FKAC165"      () (copy zip directory "FKAC165"))
			("FKAC167"      () (copy zip directory "FKAC167"))
			("FKAC168"      () (copy zip directory "FKAC168"))
			("FKCA172"      () (copy zip directory "FKCA172"))
			("FKAC522.+ERR" () (copy zip directory "FKAC522_ERR"))
			("FKAC053.+ERR" () (copy zip directory "FKAC053_ERR"))
			("FKBD053.+ERR" () (copy zip directory "FKBD053_ERR"))
			("FKBB331.+ERR" () (copy zip directory "FKBB331_ERR"))
			("\\d{10}_00263129_\\d{9}_[0-9]"
			 () (copy zip directory "MAIN"))
			("ke26312901_\\d{14}"
			 () (copy zip directory "ke26312901"))))))

;; #P"f:/zip/MAIN/2620700092_00263129_200810230_1.zip"
(defun zip-info (zipfile)
  (let1 contents (car (kensin::kx/parse2 zipfile))
    (list (if (second contents)
	      (cl-irregsexp:if-match-bind ((header (or #\h #\g)) _) (the string (second contents))
					  (if (equal header "h") :kensin :hsido)
					  :other)
	      :other)
	  (aif (last1 contents)
	       (nendo-year it) nil))))

;; for-debug
;; (defun ren (l y)
;;   (print (list l y)))

(defun main-build (directory)
  (util::stdout "~Aの健診・保健指導ファイルを整理します。~%" directory)
  (util::make-directory (path+ directory "/HSIDO/"))
  (iter (for zip :in-directory directory :type "zip")
	(for info = (zip-info zip))
	(if (cadr info)
	    (progn
	      (let1 newdir (path+ directory (format nil "/~A/" (or (cadr info) "不明")))
		(util::make-directory newdir)
		(unless (cl-fad:file-exists-p (path+ newdir zip))
		  (rename-file zip (path+ newdir zip)))))
	    (progn
	      (let1 newdir (path+ directory "/HSIDO/")
		(util::make-directory newdir)
		(rename-file zip (path+ newdir zip))))))
  (util::stdout "~Aを整理しました。~%" directory))

(defmacro when-file-exist (&rest files)
  `(and ,@(mapcar (lambda (f) `(file-exists-p ,f)) files)))

(defun make-hash-thread (directory)
  (make-thread (lambda () (exist-hash directory))))

(defun move ()
  (if (when-file-exist #P"d:/" #P"f:/" #P"g:/")
      (let ((f-thread (make-hash-thread #P"f:/zip/"))
	    (d-thread (make-hash-thread #P"d:/zip/")))
	(dir/comp #P"y:/23吉田/未処理/" #P"d:/zip/" d-thread :type :rename)
	(dir/comp #P"g:/" #P"d:/zip/" d-thread)
	(dir/comp #P"d:/zip/" #P"f:/zip/" f-thread)
	(dir/comp #P"f:/zip/" #P"d:/zip/" d-thread)
	(make-thread
	 (lambda ()
	   (directory-classify #P"d:/zip/")
	   (main-build #P"d:/zip/MAIN/")))
	(make-thread
	 (lambda ()
	   (directory-classify #P"f:/zip/")
	   (main-build #P"f:/zip/MAIN/"))))))
(in-package :cl-user)

