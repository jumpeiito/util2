(in-package :hsido)

(defparameter topdir
  (util:path+ (util:exists kensin/setting::hsido-directory)
	      "/HSIDO/"))

(defstruct (hsido
	     (:constructor hsidoj (zipname xmlname jnumber kigo bango name birth hcode
					   hname created occurd rnumber level stage cource firstd performer
					   finald)))
  zipname xmlname jnumber kigo bango name birth hcode
  hname created occurd rnumber level stage cource firstd performer
  finald hit 172hit2012 172hit2011 hmes2012 hmes2011 kmes2012 kmes2011)

(defun hsido-file? (pathname)
  (zip:with-zipfile (z pathname)
    (zip:do-zipfile-entries (k e z)
      (cl-irregsexp:if-match-bind
       ((zipname (string 31)) "/DATA/" (name (* string)))
       (the string k)
       (cl-irregsexp:if-match-bind
	((type (string)) (+ string))
	(the string name)
	(return (equal "g" type)))))))

(defun internal-init ()
  (iter (for zip :in-directory topdir :type "zip")
	(appending (kensin::hs/parse-zip-main zip))))

(defmacro with-hsido-slot (instance &body body)
  `(with-slots (zipname xmlname jnumber kigo bango name birth hcode
			hname created occurd rnumber level stage cource firstd performer
			finald hit 172hit2012 172hit2011 hmes2012 hmes2011 kmes2012 kmes2011)
       ,instance
     ,@body))

(defun repair-jnumber (jnumber)
  (cl-irregsexp:if-match-bind ((nendo (string 2)) (string)
			       (other (+ (string))))
			      (the string jnumber)
			      (the string (format nil "~A1~A" nendo other))))

(defmacro hash-hit (&rest args)
  `(or ,@(mapcar
	  (lambda (l)
	    `(and ,(first l)
		  (not (equal "" ,(first l)))
		  (gethash ,(first l) ,(second l))))
	  args)))

(defmacro get-message (func database)
  `(if ,database
       (funcall ,func (car ,database))
       ""))

(defun create-hsido (list hash1 hash2 hash3 hash4 172hash2012 172hash2011)
  (let1 obj (apply #'hsidoj list)
    (with-hsido-slot obj
      (setq jnumber	(repair-jnumber jnumber)
	    birth	(normal-date->string (strdt birth))
	    hname	(car (gethash hcode kensin::hospital-short-hash))
	    created	(normal-date->string (strdt created))
	    occurd	(normal-date->string (strdt occurd))
	    finald	(normal-date->string (strdt finald))
	    hit		(hash-hit (jnumber hash1)
				  (jnumber hash2)
				  (jnumber hash3)
				  (jnumber hash4))
	    172hit2012	(hash-hit (jnumber 172hash2012)
				  (rnumber 172hash2012))
	    172hit2011	(hash-hit (jnumber 172hash2011)
				  (rnumber 172hash2011))
	    kmes2012	(get-message #'kensin::172data-健診メッセージ
				     172hit2012)
	    kmes2011	(get-message #'kensin::172data-健診メッセージ
				     172hit2011)
	    hmes2012	(get-message #'kensin::172data-指導メッセージ
				     172hit2012)
	    hmes2011	(get-message #'kensin::172data-指導メッセージ
				     172hit2011)))
    obj))

(defun collect ()
  (iter (with 2010jhash = (kensin::165jhash kensin::2010csv))
	(with 2011jhash = (kensin::165jhash kensin::2011csv))
	(with 2010rhash = (kensin::165rhash kensin::2010csv))
	(with 2011rhash = (kensin::165rhash kensin::2011csv))
	(with 172hash2012 = (kensin::172-hash))
	(with 172hash2011 = (kensin::172-2011-hash))
	(for l :in (internal-init))
	(for jnum = (third l))
	(if (or (ppcre:scan "^11" jnum)
		(ppcre:scan "^12" jnum)
		(ppcre:scan "^13" jnum))
	    (iter:collect
		(handler-case (create-hsido l 2010jhash 2011jhash 2010rhash 2011rhash 172hash2012 172hash2011)
		  (#+sbcl  sb-int:simple-program-error
		   #+clisp system::simple-program-error (e)
		    (declare (ignorable e))
		    (print (length l))))))))

(defmethod final? ((h hsido))
  (not (string-null (hsido-finald h))))

(defmethod first? ((h hsido))
  (and (or (not (hsido-finald h)) (string-null (hsido-finald h)))
       (not (string-null (hsido-firstd h)))))

(defun dependency-initial (collecter)
  (iter (for c :in collecter)
	(with-hsido-slot c
	  (phash c
		 :condition t
		 :key #'hsido-jnumber))))

(defun keyfn (list)
  (hsido-zipname (find-if #'final? list)))

(defun valfn (list)
  (hsido-zipname (find-if #'first? list)))

(defun dependency (collecter)
  (iter (with hash = (make-hash-table :test #'equal))
	(for (k v) :in-hashtable (dependency-initial collecter))
	(if (>= (length v) 2)
	    (setf (gethash (keyfn v) hash)
	    	  (uniq (cons (valfn v) (gethash (keyfn v) hash)))))
	(finally (return hash))))

(defmethod formatta1 ((h hsido))
  (with-hsido-slot h
    (list jnumber kigo bango name birth hname
	  created occurd rnumber firstd finald
	  (if hit "○" "×")
	  (if 172hit2012 "○" "×")
	  (if 172hit2011 "○" "×")
	  kmes2012 kmes2011 hmes2012 hmes2011)))

(defmethod formatta2 ((h hsido))
  (with-hsido-slot h
    (list zipname xmlname hname jnumber rnumber kigo bango name
	  occurd firstd finald
	  (if hit "○" "×")
	  (if 172hit2012 "○" "×")
	  (if 172hit2011 "○" "×"))))

(defun title1 ()
  (list "受診券番号" "記号" "番号" "名前" "生年月日" "医療機関"
	"作成日" "実施日" "利用券番号" "初回日" "評価日" "アップ済" "2012年172" "2011年172"
	"指導M2012" "指導M2011" "健診M2012" "健診M2011"))

(defun title2 ()
  (list "zip" "xml" "院所" "受診券番号" "利用券番号" "記号" "番号" "名前"
	"実施日" "初回" "最終" "アップ済" "172-2012" "172-2011"))

(defun titlefn (arg)
  (case arg
    (1 #'title1)
    (2 #'title2)))

(defun formattafn (arg)
  (case arg
    (1 #'formatta1)
    (2 #'formatta2)))

(defun output (&key (type 1) (file "test.csv"))
  (call-with-output-file2 file
    (lambda (op)
      (format op "~{~A~^,~}~%" (funcall (titlefn type)))
      (iter (for c :in (collect))
	    (format op "~{~A~^,~}~%" (funcall (formattafn type) c))))
    :code :SJIS))

(defun internal-upload-file-collect (collecter)
  (iter (for c :in collecter)
	(phash c
	       :condition t
	       :key   #'hsido-zipname)))

(defun internal-uploader (collecter)
  (iter (with dhash = (dependency collecter))
	(for (k v) :in-hashtable (internal-upload-file-collect collecter))
	(if (and
	     ;; (1) 全員が健診結果にアップロードされている	かつ
	     (every #'hsido-172hit2012 v)
	     ;; (2) 全員利用券番号が入っている			かつ
	     (every (compose #'util::string-not-null #'hsido-rnumber) v)
	     ;; (3) ファイルの依存関係が解決している
	     (or (not (gethash k dhash nil))
		 nil))
	    (collect (list k v)))))

(defun upload-files (collecter)
  (iter (for (filename . data) :in (internal-uploader collecter))
	(collect (make-pathname :defaults topdir
				:name filename
				:type "zip"))))

(defun upload-number (collecter)
  (iter (for (filename . data) :in (internal-uploader collecter))
	(appending (mapcar #'hsido-rnumber (car data)) :into pot)
	(finally (return (remove-if #'util::string-null (uniq pot))))))

(defun file-to-331file (zipfiles)
  (iter (for zip :in zipfiles)
	(appending (iter (for line :in (kensin::hs/parse-zip-main zip))
			 (collect (third line)))
		   :into pot)
	(finally (kensin::make-331-file pot))))

(defun drive-g-non-existance-error ()
  (unless (cl-fad:file-exists-p #P"g:/")
    (error "連合会用USBメモリ(白色)を挿してください。")))

(defun directory-for-upload ()
  (format nil "g:/~A_up/" (util::today-8)))

(defun upload ()
  (DRIVE-G-NON-EXISTANCE-ERROR)

  (make-directory (DIRECTORY-FOR-UPLOAD))

  (let1 collection (collect)
    (iter (for c :in (upload-files collection))
	  (cl-fad:copy-file c
			    (path+ (DIRECTORY-FOR-UPLOAD) c)))

    (kensin::make-331-file (upload-number collection))

    (cl-fad:copy-file "FKBB331.csv"
		      (path+ (directory-for-upload) "FKBB331.csv"))))

;; (kensin::make-331-file '("12211200450"
;; 			 "12212024201"
;; 			 "12218007201"
;; 			 "12218023201"
;; 			 "12218056501"
;; 			 "12218060401"
;; 			 "12218060501"
;; 			 "12218068301"
;; 			 "12218068501"
;; 			 "12218078801"
;; 			 "12285108501"
;; 			 "12285111101"
;; 			 "12285122001"
;; 			 "12295134701"
;; 			 "12295145701"
;; 			 "12295161501"
;; 			 "12312010401"
;; 			 "12312011201"
;; 			 "12312021801"
;; 			 "12312027901"
;; 			 "12318028001"
;; 			 "12318049501"
;; 			 "12385093302"
;; 			 "12385108502"
;; 			 "12395142701"
;; 			 "12395155101"
;; 			 "12395166501"
;; 			 "12395169601"
;; 			 "12395196801"))

(in-package :cl-user)
