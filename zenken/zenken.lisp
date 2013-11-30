(in-package :zenken)

(declaim (inline chomp create-zenken))

(defparameter %file ;#P"f:/20130628/特定健診全件データ.csv"
  (or (cl-fad:file-exists-p #P"d:/特定健診システム/特定健診CSV/特定健診全件データ.csv")
      (cl-fad:file-exists-p #P"f:/20130628/特定健診全件データ.csv")))
(defparameter %directory '(#P"d:/特定健診システム/特定健診CSV/"
		     #P"f:/20130628/"))

;; (defparameter za
;;   (remove-if
;;    (lambda (obj)
;;      (with-slots (年度末年齢 整理番号 途中取得日 途中喪失日) obj
;;        (or (<= (read-from-string 年度末年齢) 39)
;; 	   (>= (read-from-string 年度末年齢) 75)
;; 	   (equal "00000000000" 整理番号)
;; 	   途中取得日
;; 	   途中喪失日
;; 	   (not (ppcre:scan "^12" 整理番号)))
;;        ))
;;    (to-data %file)))

;; (iter (for z :in za)
;;       (phash z :condition t :key #'zenken::zenken-個人番号))

(defparameter years '(2008 2009 2010 2011 (2012 . "特定健診全件データ.csv") 2013))

(defmacro select (name)
  `(merge-pathnames (find-if #'file-exists-p %directory)
		    ,name))

(defun generate-files ()
  (iter (for y :in years)
	(if (atom y)
	    (collect (cons y (select (format nil "~A特定健診全件データ.csv" y))))
	    (collect (cons (car y) (select (cdr y)))))))

(defstruct (zenken
	     (:constructor zenken-gen
			   (年度末支部 名 保険証番号 行 氏名 本人／家族
性別 生年月日 年度末年齢 途中取得日
途中喪失日 除外 整理番号 発行日 受診日
健診機関 取込方法 保健指導 指導日 支援
証支部 個人番号 組合番号 支部 分会 班 表示順))
	     (:constructor make-zenken (支部)))
  年度末支部 名 保険証番号 行 氏名 本人／家族 性別 生年月日 年度末年齢 途中取得日 途中喪失日
  除外 整理番号 発行日 受診日 健診機関 取込方法 保健指導 指導日 支援 証支部 個人番号 組合番号
  支部 分会 班 表示順 上名 下名 分会名 現支部 健診機関コード)

;; (defun chomp (str)
;;   (cl-ppcre:regex-replace-all "[　 ]+$" str ""))

(defun chomp (str)
  (labels ((in (subs)
	     (if (equal subs "")
		 ""
		 (if (or (char-equal (aref subs 0) #\ )
			 (char-equal (aref subs 0) #\　))
		     (in (subseq subs 1 (length subs)))
		     (reverse subs)))))
    (in (reverse str))))

(defparameter x (make-list 100000 :initial-element "20130714"))


(defmacro with-zenken-slot (instance &rest body)
  `(with-slots (年度末支部 名 保険証番号 行 氏名 本人／家族 性別 生年月日
			  年度末年齢 途中取得日 途中喪失日 除外 整理番号 発行日 受診日
			  健診機関 取込方法 保健指導 指導日 支援 証支部 個人番号 組合番号
			  支部 分会 班 表示順 上名 下名 分会名 現支部 健診機関コード) ,instance
     ,@body))

(defun create-zenken (list bhash)
  (declare (type list list) (type hash-table bhash)
	   ;; (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
	   (optimize (speed 0) (safety 3) (debug 3) (compilation-speed 0) (space 0)))
  (let1 obj (apply #'zenken-gen list)
    (with-zenken-slot obj
      (setq 氏名	(chomp 氏名)
	    取込方法	(chomp 取込方法)
	    保健指導	(chomp 保健指導)
	    支援	(chomp 支援)
	    生年月日	(date-string-normalize (strdt 生年月日))
	    途中取得日	(date-string-normalize (strdt 途中取得日))
	    途中喪失日	(date-string-normalize (strdt 途中喪失日))
	    発行日	(date-string-normalize (strdt 発行日))
	    受診日	(date-string-normalize (strdt 受診日))
	    指導日	(date-string-normalize (strdt 指導日))
	    分会名	(gethash (format nil "~A~A" 証支部 分会) bhash)
	    現支部	(if (equal "85" 証支部)
			    (subseq (the string 保険証番号) 4 6)
			    証支部)
	    健診機関コード 健診機関
	    健診機関	(or (car (gethash 健診機関 kensin:hospital-short-hash nil))
			    (second (gethash 健診機関 kensin:dock-hash nil))
			    ""))
      ;; (cl-irregsexp:if-match-bind
      ;;  ((f (+ (string))) (the string "　") (u (+ (string))))
      ;;  (the string 氏名)
      ;;  (setq 上名 (the string f)
      ;; 	     下名 (the string u)))
      (the zenken obj))))

(defun target? (obj)
  (declare (type zenken obj)
	   ;; (optimize (speed 3) (safety 0) (debug 0))
	   (optimize (speed 0) (safety 3) (debug 3)))
  (optima:match obj
    ((zenken 年度末年齢 途中取得日 途中喪失日)
     (let1 year (the integer (read-from-string 年度末年齢))
       (and (>= year 40) (< year 75)
	    (not 途中取得日) (not 途中喪失日))))))

(defun to-data (filename)
  (iter (with bhash = (kensin:bunkai-hash))
	(for line :in-csv filename :code :SJIS)
	(unless (first-time-p)
	  (collect (create-zenken line bhash)))))


(defmacro defhash (defname keyfn)
  `(defun ,defname (filename &key (verbose t))
     (iter (with list    = (to-data filename))
	   (for  line    :in list)
	   (phash line :condition t
		       :key ,keyfn))))

(defhash make-hash	    (lambda (o) (normal-date->string (strdt (zenken-生年月日 o)))))
(defhash make-name-hash     #'zenken-氏名)
;; (defhash make-jusinken-hash #'zenken-整理番号)
(defun make-jusinken-hash (file)
  (iter (with bhash = (kensin:bunkai-hash))
	(with hash = (make-hash-table :test #'equal))
	(for line :in-csv file :code :SJIS)
	(when (first-time-p) (next-iteration))
	(optima:match line
	  ((list* _ _ _ _ _ _ _ _ _ _ _ _ "00000000000" rest)
	   (next-iteration))
	  ((list* _ _ _ _ _ _ _ _ _ _ _ _ jnum rest)
	   (setf (gethash jnum hash) (create-zenken line bhash))))
	(finally (return hash))))


(defun make-jusinken-simple-hash (filename)
  (iter (for line :in (to-data filename))
	(shash line
	       :value     #'zenken-整理番号
	       :key       #'zenken-個人番号
	       :condition t;; (>= (read-from-string (zenken-年度末年齢 line)) 40)
	       )))

(defun id-hash (filename basedate)
  (iter (for line :in (to-data filename))
	(with-zenken-slot line
	  ;; (if (and (>= (how-old 生年月日 basedate) 40)
	  ;; 	   (<= (how-old 生年月日 basedate) 75))
	  ;;     (shash line
	  ;; 	     :condition t
	  ;; 	     :key   #'zenken-個人番号
	  ;; 	     :value (lambda (o)
	  ;; 		      (with-zenken-slot o
	  ;; 			(list 氏名 受診日
	  ;; 			      (if (equal 健診機関コード "0000000000") nil 健診機関コード)
	  ;; 			      (if (string-null 健診機関) nil 健診機関)
	  ;; 			      (if (string-null 取込方法) nil 取込方法)
	  ;; 			      発行日 分会 班 分会名 現支部
	  ;; 			      年度末支部 整理番号)))))
	  (shash line
	  	 :condition t
	  	 :key   #'zenken-個人番号
	  	 :value (lambda (o)
	  		  (with-zenken-slot o
	  		    (list 氏名 受診日
	  			  (if (equal 健診機関コード "0000000000") nil 健診機関コード)
	  			  (if (string-null 健診機関) nil 健診機関)
	  			  (if (string-null 取込方法) nil 取込方法)
	  			  発行日 分会 班 分会名 現支部
	  			  年度末支部 整理番号))))
	  )))

;; (defun complex-hash (filename)
;;   (iter (for line :in (to-data filename))
;; 	(phash line
;; 	       :condition t
;; 	       :key   (lambda (o) (with-zenken-slot o (list 氏名 生年月日)))
;; 	       :value (lambda (o)
;; 			(with-zenken-slot o
;; 			  (list 保険証番号 生年月日 氏名 途中取得日 途中喪失日
;; 				受診日 健診機関 支部 分会 班 分会名 現支部))))))

;; (defun complex-hash2 (filename)
;;   (iter (for line :in (to-data filename))
;; 	(phash line
;; 	       :condition t
;; 	       :key   (lambda (o) (with-zenken-slot o (list 氏名 生年月日)))
;; 	       :value (lambda (o)
;; 			(with-zenken-slot o
;; 			  (list 整理番号 保険証番号 生年月日 氏名 途中取得日 途中喪失日
;; 				受診日 健診機関 支部 分会 班 分会名 現支部))))))

(defun hash () (make-hash %file))

(defmacro zfind (test &rest body)
  `(iter (for zenken :in (to-data %file))
	 (if (with-zenken-slot zenken ,test)
	     (with-zenken-slot zenken
	       (progn
		 ,@body)))))

(defun string-space-filled? (string)
  (declare (type simple-string string)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((in (subs)
	     (if (string-null subs)
		 t
		 (let1 head (the character (aref subs 0))
		   (if (or (char-equal head #\ )
			   (char-equal head #\　))
		       (in (the string (subseq subs 1 (length subs))))
		       nil)))))
    (if (string-null string)
	nil
	(in string))))

(declaim (inline kensin:kensin-year? string-space-filled?))

(defun month-calc (file)
  (iter (with array = (make-array '(96 13) :initial-element nil))
	(for line :in-csv file :code :SJIS)
	(if (first-time-p) (next-iteration))
	(cl-match:match line
	  ((list* shibu _ _ _ _ _ _ _ o g r _ _ _ date hospital _)
	   (if (and (kensin:kensin-year? (read-from-string o))
		    (string-space-filled? g)
		    (string-space-filled? r)
		    (not (equal hospital "0000000000")))
	       (let* ((m     (date-month (strdt date)))
		      (month (if (< m 4) (+ m 9) (- m 4))))
		 (setf (aref array (read-from-string shibu) month)
		       (cons hospital (aref array (read-from-string shibu) month)))))))
	(finally (return array))))

(in-package :cl-user)
