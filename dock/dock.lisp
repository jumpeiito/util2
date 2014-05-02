(in-package :dock)

(defparameter datafile    (merge-pathnames ks::dock-directory "ysddata"))
(defparameter unregfile   (merge-pathnames ks::dock-directory "unreg"))
;; (defparameter uplog-hash  (uplog::uplog-hash))

(defun search-path ()
  (iter (for year :upfrom (read-from-string ks::year))
	(let1 newdir (format nil "~A~A/" ks::dock-directory year)
	  (if (file-exists-p newdir)
	      (collect newdir)
	      (finish)))))

;; (defun zip-dock-type? (pathname)
;;   (cl-irregsexp:if-match-bind
;;    ((string 10) "_" (string 8) "_" (string 9) "_" (string))
;;    (the string (pathname-name pathname))))

(defstruct (dock
	     (:constructor ysd-gen (予約日 健診機関 保険番号 氏名 生年月日 整理番号 半日 脳 肺 ペット 受診日 費用 支払日 支払月)))
  予約日 健診機関 保険番号 氏名 生年月日 整理番号 半日 脳 肺 ペット
  受診日 費用 支払日 支払月 支部 連合会 年度 ファイル 全件 脳単独 予約年度)

(defmacro with-dock-slots (instance &rest body)
  `(with-slots (予約日 健診機関 保険番号 氏名 生年月日 整理番号
		       半日 脳 肺 ペット 受診日 費用 支払日
		       支払月 支部 連合会 年度 ファイル 全件
		       脳単独 予約年度)
       ,instance
     ,@body))

(defparameter seekfile ksetting::*zip-parse-file*)

(defun seek-birthday-hash (file)
  (iter (for line :in-csv file :code :SJIS)
	(phash line
	       :condition t
	       :key #'seventh)))

(defun seek-hcode-hash (file)
  (iter (for line :in-csv file :code :SJIS)
	(phash line
	       :condition t
	       :key #'eighth)))

(defun seek-birthday (birthday)
  (gethash (util::normal-date->string (util::strdt birthday))
	   (seek-birthday-hash seekfile)))

(defun seek-hcode (hcode)
  (gethash hcode (seek-hcode-hash seekfile)))

(defun seek-hname (hname)
  (iter (for line :in-csv seekfile :code :SJIS)
	(if (ppcre:scan hname (ninth line))
	    (collect line))))

(defmacro seek-function (list)
  `(lambda (line)
     (and ,@(iter (for pair :in (group list 2))
		  (if (second pair)
		      (collect (case (first pair)
				 (:birth `(if ,(second pair)
					      (equal (normal-date->string (strdt ,(second pair))) (nth 6 line))
					      t))
				 (:name  `(if ,(second pair)
					      (ppcre:scan ,(second pair) (sixth line))
					      t))
				 (:hname `(if ,(second pair)
					      (ppcre:scan ,(second pair) (ninth line))
					     t))
				 (:nendo `(if ,(second pair)
					      (equal ,(second pair) (nendo-year (strdt (last1 line))))
					      t)))))))))

(defun seek (&key (birth nil) (name nil) (hname nil) (nendo nil))
  (iter (for line :in-csv seekfile :code :SJIS)
	(if (funcall (seek-function (:birth birth :name name :hname hname :nendo nendo)) line)
	    (collect line))))

(defmethod brain-single-p ((d dock))
  (with-dock-slots d
    (and (or (not 半日) (string-null 半日))
	 脳 (not (string-null 脳)))))

(defun make-shibu (kb)
  (cl-irregsexp:if-match-bind
   ((shibu (string 2)) (other (string 5)))
   (the string kb)
   (if (equal "85" shibu)
       (cl-irregsexp:if-match-bind
	((scode (string 2)) (other2 (string 3)))
	(the string other)
	(values (format nil "~A~A" (kensin:short-shibu shibu) other)
		(kensin::long-shibu scode))
	(values nil nil))
       (values (format nil "~A~A" (kensin:short-shibu shibu) other)
	       (kensin::long-shibu shibu)))))

(defmacro date-normal (val)
  `(if (and ,val (not (string-null ,val)))
       (date-string-normalize (strdt ,val))
       nil))

(defun create-dock (list 172hash xhash)
  (let1 obj (apply #'ysd-gen list)
    (with-dock-slots obj
      (setq 予約日	(date-normal 予約日)
	    生年月日	(date-normal 生年月日)
	    受診日	(date-normal 受診日)
	    支払日	(date-normal 支払日)
	    連合会	(gethash 整理番号 172hash nil)
	    年度	(nendo-year 受診日)
	    ファイル	(gethash 整理番号 xhash nil)
	    脳単独	(brain-single-p obj)
	    予約年度	(nendo-year 予約日))
      (multiple-value-bind (kb shibu) (make-shibu 保険番号)
	(setq 保険番号	kb
	      支部	shibu)))
    obj))

(defmethod write-down ((d dock) op)
  (with-dock-slots d
    (format op
	    "~{~A~^,~}~%"
	    (list 健診機関 (or 予約日 "") (or 受診日 "") (or 年度 "")
		  (or 支払日 "") (if 脳単独 "脳単独" "")
		  保険番号 氏名 生年月日
		  (util::how-old 生年月日 (nendo-end (read-from-string ks::year)))
		  (if 連合会 "○" "×")
		  (if ファイル "○" "×")))))

(defmethod write-down2 ((d dock) op)
  (with-dock-slots d
    (format op
	    "~{~A~^,~}~%"
	    (list 健診機関 (or 予約日 "") (or 受診日 "") (or 年度 "")
		  (or 支払日 "") (if 脳単独 "脳単独" "")
		  保険番号 氏名 生年月日
		  (util::how-old 生年月日 (nendo-end (read-from-string ks::year)))
		  (if 連合会 "○" "×")
		  (if ファイル "○" "×")
		  (if ファイル (car ファイル) "")))))

(defun %collection ()
  (iter (for dir :in (search-path))
  	(appending (allf dir :type "zip" :regexp "\\d{10}_\\d{8}_\\d{9}_[0-9]"))))

;; (defun parse ()
;;   (iter (with collection = (%collection))
;; 	(for file :in collection)
;; 	(for contents = ;; (kensin:kx/parse2 file)
;; 	     (mapcar #'kxml::info (kxml::parse-zipfile file)))
;; 	(appending (iter (for line :in contents)
;; 			 (collect line)))))

(defun parse1 (list)
  (iter (for file :in list)
	(for contents = (mapcar #'kxml::info (kxml::parse-zipfile file)))
	(appending (iter (for line :in contents)
			 (collect line)))))

(defun list-split (l n)
  (let ((length (1+ (truncate (/ (length l) n)))))
    (labels ((inner (subl r)
	       (if (null subl)
		   (reverse r)
		   (inner (drop subl length) (cons (take subl length) r)))))
      (inner l nil))))

(defmacro %parse (n)
  (let ((gensym-list (mapcar (lambda (_) (declare (ignore _)) (gensym))
			     (iota :from 1 :to n)))
	(list (gensym)))
    `(let* ((,list (list-split (%collection) ,n))
	    ,@(mapcar-with-index
	       (lambda (i gensym)
		 `(,gensym (sb-thread::make-thread (lambda () (parse1 (nth ,i ,list))))))
	       gensym-list))
       (append ,@(mapcar
		  (lambda (g)
		    `(sb-thread::join-thread ,g))
		  gensym-list)))))

;; (%parse 6)

(defun parse ()
  (let* ((l  (list-split (%collection) 6))
	 (f1 (sb-thread::make-thread (lambda () (parse1 (first l)))))
	 (f2 (sb-thread::make-thread (lambda () (parse1 (second l)))))
	 (f3 (sb-thread::make-thread (lambda () (parse1 (third l)))))
	 (f4 (sb-thread::make-thread (lambda () (parse1 (fourth l)))))
	 (f5 (sb-thread::make-thread (lambda () (parse1 (fifth l)))))
	 (f6 (sb-thread::make-thread (lambda () (parse1 (sixth l))))))
    (append (sb-thread::join-thread f1)
	    (sb-thread::join-thread f2)
	    (sb-thread::join-thread f3)
	    (sb-thread::join-thread f4)
	    (sb-thread::join-thread f5)
	    (sb-thread::join-thread f6))))

;; #|
;; (kensin:kx/parse2 #P"f:/zip/MAIN/2013/2619700129_00263129_201309090_1.zip")
;; Evaluation took:
;;   0.016 seconds of real time
;;   0.015600 seconds of total run time (0.015600 user, 0.000000 system)
;;   100.00% CPU
;;   68,638,645 processor cycles
;;   1,558,128 bytes consed

;; (parse)
;; Evaluation took:
;;   5.895 seconds of real time
;;   5.335235 seconds of total run time (4.976432 user, 0.358803 system)
;;   [ Run times consist of 0.299 seconds GC time, and 5.037 seconds non-GC time. ]
;;   90.50% CPU
;;   18,239,602,690 processor cycles
;;   597,551,624 bytes consed

;; |#

;; #|
;; (time (mapcar #'kxml::info (kxml:parse-zipfile #P"f:/zip/MAIN/2013/2619700129_00263129_201309090_1.zip")))
;; Evaluation took:
;;   0.015 seconds of real time
;;   0.015600 seconds of total run time (0.015600 user, 0.000000 system)
;;   106.67% CPU
;;   71,299,846 processor cycles
;;   1,546,576 bytes consed

;; (parse)
;; Evaluation took:
;;   5.879 seconds of real time
;;   5.366434 seconds of total run time (5.007632 user, 0.358802 system)
;;   [ Run times consist of 0.330 seconds GC time, and 5.037 seconds non-GC time. ]
;;   91.27% CPU
;;   18,170,302,164 processor cycles
;;   594,443,280 bytes consed
;; |#
(defun compute ()
  (iter (for data :in (parse))
	(shash data :condition t :key #'third)))

(defun file-to-contents (pathname)
  (call-with-input-file2 pathname
    (lambda (ip)
      (loop for i = (read ip nil nil nil)
	 while i
	 collect i))
    :code (file-coding pathname)))

(defun unregistry (&rest args)
  (declare (ignorable args))
  (iter (for line :in (file-to-contents unregfile))
	(shash line :condition t
	       :key (lambda (l) (list (seventh l))))))

;; (defmacro with-obj-slots (instance &body body)
;;   (labels ((slots (f) (mapcar #'c2mop:slot-definition-name
;; 			      (c2mop::class-slots (class-of f)))))
;;     (slots instance)))
(defun kensin-old? (d)
  (declare (type dock d))
  (with-slots (生年月日 整理番号) d
    (let ((y (kensin::jnum-how-old 生年月日 整理番号)))
      (and (>= y 40)
	   (<  y 75)))))

(defun payed? (d)
  (dock-支払日 d))

(defun brain-single? (d)
  (dock-脳単独 d))

(defun kensin-year? (d)
  (or (and (dock-予約年度 d)
	   (= (dock-予約年度 d) ksetting::*year*))
      (and (dock-年度 d)
	   (= (dock-年度 d) ksetting::*year*))))

(defun kensin-year=? (d)
  (and (dock-年度 d)
       (eq (dock-年度 d) ksetting::*year*)))

(defun has-file? (d)
  (dock-ファイル d))

(defun uploaded? (d)
  (dock-連合会 d))

(defun got? (d)
  (with-slots (全件) d
    (and 全件
	 (zenken::zenken-途中取得日 全件))))

(defun lost? (d)
  (with-slots (全件) d
    (and 全件
	 (zenken::zenken-途中喪失日 全件))))

(defun occur? (d)
  (dock-受診日 d))

(defun zip (d)
  (aif (dock-ファイル d) (car it) nil))

(defun xml (d)
  (aif (dock-ファイル d) (second it) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xx? (d)
  (and (payed? d)
       (not (brain-single? d))
       (kensin-year? d)
       (kensin-old? d)
       (not (has-file? d))
       (not (uploaded? d))))

(defun xo? (d)
  (and (payed? d)
       (not (brain-single? d))
       (kensin-year=? d)
       (kensin-old? d)
       (has-file? d)
       (not (uploaded? d))))

(defun whole (d)
  (and ;; (occur? d)
       (kensin-year? d)))

(defun %ZenkenHash ()
  (cl-store:restore ksetting::*zenken-hash*))

(defun %XmlHash ()
  (iter (for line :in-csv ksetting::*zip-parse-file* :code :SJIS)
	(shash line
	       :condition t
	       :key #'third)))

(defun %create (line 172hash xhash zhash zhash2)
  (let1 obj (create-dock line 172hash xhash)
    (with-slots (全件 整理番号) obj
      (if (eq (length 整理番号) 11)
	  (setq 全件 (gethash 整理番号 zhash))
	  (setq 全件 (gethash (format nil "~9,,,'0@A" 整理番号) zhash2)))
      obj)))

(defparameter title
  (format nil "~{~A~^,~}"
	  '("健診機関" "予約日" "受診日" "年度" "支払日" "脳単独"
	    "整理番号" "保険番号" "氏名" "生年月日" "UP済" "ファイル"
	    "途中取得日" "途中喪失日" "zip名" "xml名")))

(defun tolist (d)
  (declare (type dock d)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (健診機関 予約日 受診日 年度 支払日 脳単独 整理番号 保険番号 氏名 生年月日 全件) d
    (mapcar (lambda (el) (or el ""))
	    (list 健診機関 予約日 受診日 年度 支払日
		  (if 脳単独 "脳単独" "")
		  整理番号 保険番号 氏名 生年月日
		  (if (uploaded? d) "○" "×")
		  (if (has-file? d) "○" "×")
		  (if 全件 (zenken::zenken-途中取得日 全件))
		  (if 全件 (zenken::zenken-途中喪失日 全件))
		  (zip d)
		  (xml d)))))

(defun tostring (d)
  (declare (type dock d)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (format nil "~{~A~^,~}" (tolist d)))

(defun collection (f)
  (iter (with 172hash = (kensin::172-hash))
	(with xhash   = (%XmlHash))
	(with zhash   = (%ZenkenHash))
	(with zhash2  = (zenken::id-hash3))
	(for line :in-csv ksetting::*dock-output-file* :code :UTF-8)
	(for obj = (%create line 172hash xhash zhash zhash2))
	(if (funcall f obj)
	    (collect obj :into pot))
	(finally (return ;; (sort2 pot string< dock-健診機関)
		   (multiple-sort pot
				  (string< dock-健診機関)
				  (string< dock-支払日)
				  (string< dock-受診日))))))

(defparameter title
  (format nil "~{~A~^,~}"
	  '("健診機関" "予約日" "受診日" "年度" "支払日" "脳単独"
	    "整理番号" "保険番号" "氏名" "生年月日" "UP済" "ファイル"
	    "途中取得日" "途中喪失日" "zip名" "xml名")))

(defun tocsv ()
  (call-with-output-file2 ksetting::*dock-init-file2*
    (lambda (op)
      (write-line title op)
      (iter (for line :in (collection #'whole))
	    (format op "~A~%" (tostring line))))
    :code :SJIS))

(defun tocsv-all ()
  (call-with-output-file2 ksetting::*dock-init-file2*
    (lambda (op)
      (write-line title op)
      (iter (for line :in (collection #'identity))
	    (format op "~A~%" (tostring line))))
    :code :SJIS))

(defun %condition (val)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (iter (for line :in val)
	(for row :upfrom 1)
	(declare (type fixnum row))
	(optima:match line
	  ((LIST* "健診機関" _)
	   (next-iteration))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ _ _ get lost _)
	   (if (or get lost)
	       (collect row :into bl2)
	       (optima:fail)))
	  ((LIST* _ _ _ _ _ "脳単独" _)
	   (collect row :into bl2))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ "×" "○" _)
	   (collect row :into yellow))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ "○" _)
	   (collect row :into bl2))
	  ((LIST* _ _ _ _ nil _)
	   (collect row :into bl2))
	  ((LIST* _ _ _ nendo _ _ _ _ _ birth _)
	   (if (not (kensin:kensin-year?
		     (the fixnum (how-old birth (nendo-end (truncate nendo))))))
	       (collect row :into bl2)
	       (optima:fail)))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ "×" "×" _)
	   (collect row :into red))
	  ((LIST* _ _ _ _ _ _ _ _ _ _ _ "○" _)
	   (collect row :into gray)))
	(finally (return (values bl2 gray yellow red)))))

(defmacro %color (sheet function color numlist)
  `(iter (for rows :in (compact-str ,numlist))
	 (optima:match rows
	   ((type ATOM)
	    (,function ,sheet (:a rows) (:p rows) :interior ,color))
	   ((LIST start end)
	    (,function ,sheet (:a start) (:p end) :interior ,color)))))

(defvar width #(20 11 11 5 11 6.5 12.5 9 13 11 5 5 9 9 15 15))
(defvar center '(:d :f :g :h :k :l))

(defun excel-name ()
  (make-pathname :defaults ksetting::*dock-init-file2*
		 :name "ドック全件ファイル"
		 :type "xls"))

(defun toxls ()
  (with-excel (app :visible t :quit nil)
    (with-excel-book (app bk ksetting::*dock-init-file2* :close nil)
      (let* ((sh  (ole bk :Worksheets :Item 1))
	     (val (ole sh :UsedRange  :Value))
	     (lr  (lastrow sh)))
	(excel::set-colwidth sh width)
	(set-alignment sh (:a 1) (:p 1) :horizontalAlignment excel::xlcenter)
	(iter (for col :in center)
	      (set-alignment sh (col 2) (col lr) :horizontalAlignment excel::xlcenter))
	(multiple-value-bind (bl2 gray yellow red)
	    (%condition val)
	  (%color sh set-color      13382655        red)
	  (%color sh set-color      10092543        yellow)
	  (%color sh set-colorindex excel::xlgray25 gray)
	  (%color sh set-colorindex excel::xlgray50 bl2)
	  (border sh (:a 1) (:p lr))
	  (ole sh :range (format nil "A1:P~A" lr) :AutoFilter 1)
	  (excel::save-book bk ksetting::*dock-file* :xls))))))

(in-package :cl-user)
