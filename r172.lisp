(in-package :R172c)

(defparameter 172file ksetting::*fkca172*)
(defparameter 167file ksetting::*fkac167*)

(defun create-r167 (line)
  (make-r167 :jnum (nth 9 line)
	     :date (nth 10 line)
	     :hp   (nth 11 line)
	     :mlv  (nth 48 line)
	     :hlv  (nth 49 line)))

(defun 167file-read (func)
  (csv-read-iter
   167file
   (lambda (line)
     (optima:match line
       ((LIST _) nil)
       ((LIST* _ "FKAC167" _) nil)
       ((LIST* "保険者番号" _) nil)
       (_ (funcall func line))))
   :code (file-coding 167file)))

(defun 167file-hash ()
  (let ((hash (make-hash-table :test #'equal)))
    (167file-read
     (lambda (line)
       (let1 obj (create-r167 line)
	 (with-slots (jnum) obj
	   (setf (gethash jnum hash) obj)))))
    hash))

(defun data-iterate (func)
  (let ((zhash (cl-store:restore ksetting::*zenken-hash*))
	(167hash (167file-hash)))
    (csv-read-iter
     172file
     (lambda (line)
       (optima:match line
	 ((LIST _)	      :ignore)
	 ((LIST* _ "FKCA172" _) :ignore)
	 (_ (funcall func (create-172data line zhash 167hash)))))
     :code (file-coding 172file))))

(defun 172-hash ()
  (let ((hash (make-hash-table :test #'equal)))
    (data-iterate
     (lambda (d)
       (setf (gethash (172data-受診券整理番号 d) hash) d)))
    hash))

(defun 172-uploaded? (jnumber hash)
  (optima:match (gethash jnumber hash)
    ((172data 健診メッセージ 指導メッセージ)
     (if (and (util:string-null 健診メッセージ)
	      (util:string-null 指導メッセージ))
	 :uploaded
	 (format nil "~A~A" 健診メッセージ 指導メッセージ)))
    (nil :not-uploaded)))

(defun data-filter-map (func pred)
  (let ((zhash   (cl-store:restore ksetting::*zenken-hash*))
	(167hash (167file-hash)))
    (csv-read-filter-map
     172file
     ;; func部
     (lambda (line)
       (optima:match line
	 ((LIST _)	      :ignore)	; 末端行
	 ((LIST* _ "FKCA172" _) :ignore)	; 先頭行
	 (_ (funcall func (create-172data line zhash 167hash)))))
     pred :code :SJIS)))

(defun main-condition (172data)
  (if (typep 172data 'kensin::172data)
      (with-slots (資格フラグ 健診メッセージ 指導メッセージ) 172data
	(optima:match (list 資格フラグ 健診メッセージ 指導メッセージ)
	  ((LIST "0" "" "") t)
	  (_ nil)))
      nil))

(defun mainhash ()
  (let ((hash (make-hash-table :test #'equal)))
    (data-iterate
     (lambda (l)
       (if (main-condition l)
	   (setf (gethash (172data-支部 l) hash)
		 (cons l (gethash (172data-支部 l) hash))))))
    hash))

(defmacro hash+ (key val hash)
  `(setf (gethash ,key ,hash)
	 (cons ,val (gethash ,key ,hash))))

(defun sex-year-count (list)
  (group-by-length
   list
   (lambda (d) (floor (/ (get-year d) 10)))))

(defun hash-map (func hash)
  (iter (for (k v) :in-hashtable hash)
	(setf (gethash k hash)
	      (funcall func v))
	(finally (return hash))))

(defun sex-year-classify-hash (list)
  (iter (with hash = (make-hash-table :test #'equal))
	(for d :in list)
	(hash+ :all d hash)
	(if (ppcre:scan "1$" (172data-受診券整理番号 d))
	    (hash+ :h d hash)
	    (hash+ :k d hash))
	(if (string= "1" (172data-性別 d))
	    (hash+ :m d hash)
	    (hash+ :f d hash))
	(finally (return (hash-map #'sex-year-count hash)))))

(defun make-sex-year-hash ()
  (alexandria:alist-hash-table
   (mapcar (lambda (l) (cons l (make-array 4 :initial-element 0)))
	   '(:all :h :k :m :f))
   :test #'equal))

(defun sex-year-classify-hash2 (list)
  (iter (with hash = (make-sex-year-hash))
	(for d :in list)
	))

(defun sex-year-classify (hash)
  (iter (for (k v) :in-hashtable hash)
	(setf (gethash k hash)
	      (sex-year-classify-hash v))
	(finally (return hash))))

