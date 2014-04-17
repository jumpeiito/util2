(IN-PACKAGE :HSIDO)

#|
特定保健指導管理

(1) 個人をベースにした管理
    利用券整理番号を基に管理する。アップロードできているか確認するために、
    連合会のファイルが必要になる。
(2) ファイルをベースにした管理
    ファイル名をベースに管理する。
    * どのファイルをアップロードするべきか
    * ファイルをどの順番でアップロードすればよいか を管理。

(考え方) 
AがB,Cに依存しているとき	-> ((C A) (B A))
A,BがCに依存しているとき	-> (C B A)
AがB、BがCに依存しているとき	-> ((C B) (B A))

|#


(defvar FILES		(directory-list topdir :type "zip"))
(defvar OUTPUT-FILE	ksetting::*hsido-output-file*)
(defgeneric output (obj hash op))

(defparameter *REMOVE-IF-NOT-KENSIN-YEAR* t)

(defun @write ()
  (call-with-output-file2 output-file
    (lambda (op)
      (iter (for zip :in files)
	    (mapc (lambda (line)
		    (format op "~{~A~^,~}~%" line))
		  (kensin::hs/parse-zip-main zip))))))

(defun @error-print (list)
  (match list
    ((LIST* zip xml _ k b name _ _ hosp _ occurd _)
     (format t "--------------------------------------------------~%利用券番号が入っていません。~%~A (~A)~%~A (~A実施分) ~%~A-~A ~A~%"
	     zip xml hosp occurd k b name))))

(defun this-year-jnumber? (jnumber)
  (eq (read-from-string (string-take jnumber 2))
      (mod ksetting::*year* 1000)))

(defun filter-kensin-year (list)
  (remove-if-not
   (lambda (xl)
     (handler-case (this-year-jnumber? (third xl))
       ;; 利用券番号が""のとき
       (sb-kernel:bounding-indices-bad-error (e)
	 (declare (ignore e))
	 (@error-print xl))))
   list))

(defun @read-internal ()
  ;; (call-with-input-file2 output-file
  ;;   #'read)
  (csv-read-to-list output-file))

(defun @read-filter (list)
  (funcall
   (if *remove-if-not-kensin-year*
       #'filter-kensin-year
       #'identity)
   list))

(defun @read ()
  (let ((hash (kensin:172-hash)))
    (mapcar
     (lambda (l) (make-hsido l hash))
     (@read-filter (@read-internal)))))

;; (t nil t nil) (t nil t t) -> (t nil t t)
;; 論理和
(defun xor (l1 l2)
  (labels ((in (subl1 subl2 r)
	     (if (null subl1)
		 (reverse r)
		 (in (cdr subl1) (cdr subl2) (cons (or (car subl1) (car subl2)) r)))))
    (in l1 l2 nil)))

; ----------------------------------------------------------------------
(defclass Person ()
  ((file	:initarg :file :accessor  file->)
   (jnumber	:initarg :jnumber :accessor jnumber->)
   (rnumber	:initarg :rnumber :accessor rnumber->)
   (kigo	:initarg :kigo :accessor    kigo->)
   (bango	:initarg :bango :accessor   bango->)
   (name	:initarg :name :accessor    name->)
   (birth	:initarg :birth :accessor   birth->)
   (level	:initarg :level :accessor   level->)
   (stage	:initarg :stage :accessor   stage->)
   (hit		:initarg :hit :accessor   hit->)
   (172hit	:initarg :172hit :accessor 172hit->)
   (hmes	:initarg :hmes :accessor    hmes->)
   (kmes	:initarg :kmes :accessor    kmes->)
   (sign	:initarg :kmes :accessor    sign->)
   (interrupt   :initarg :interrupt :accessor interrupt->)))

; ----------------------------------------------------------------------

(defgeneric create	(arg))
(defgeneric add		(klass struct))

(defmethod create ((h HSIDO))
  (let1 obj (make-instance 'Person)
    (setf (file-> obj)		(list h)
	  (jnumber-> obj)	(hsido-jnumber h)
	  (rnumber-> obj)	(hsido-rnumber h)
	  (kigo-> obj)		(hsido-kigo h)
	  (bango-> obj)		(hsido-bango h)
	  (name-> obj)		(hsido-name h)
	  (birth-> obj)		(hsido-birth h)
	  (level-> obj)		(hsido-level h)
	  (stage-> obj)		(hsido-stage h)
	  (hit-> obj)		(hsido-hit h)
	  (172hit-> obj)	(hsido-172hit h)
	  (hmes-> obj)		(hsido-hmes h)
	  (kmes-> obj)		(hsido-kmes h)
	  (sign-> obj)		(hsido-sign h)
	  (interrupt-> obj)	(hsido-interrupt h))
    obj))

(defun hsido-person-core (p)
#|
基礎的な保健指導情報。記号番号・利用券整理番号・氏名・
生年月日・保健指導レベルをリストにして返す。
|#
  (mapcar (lambda (f) (funcall f p))
	  (list #'kigo-> #'bango-> #'rnumber-> #'name->
		#'birth-> #'level->)))

(defun hsido-person-sign (p)
#|
情報の集積具合を表す。初回実施・継続支援・中間評価・最終評価の順。
|#
  (mapcar (lambda (sign-sym) (optima:match sign-sym (nil "×") (t "○")))
	  (sign-> p)))

(defmacro string-or (&rest args)
#|
orのstring-null版。
例 (string-or "" "B" "A") => "B"
|#
  (if (null args)
      ""
      `(if (string-null ,(car args))
	   (string-or ,@(cdr args))
	   ,(car args))))

(defun superposition (lofl)
#|
要素数が同一の「リストのリスト」において、
先頭から重ね合わせて、空白を埋めていく。
例 (superposition '(("" "") (12 "") ("" 20) (30 40))) => (12 20)
|#
  (reduce (lambda (x y)
	    (optima:match x
	      (nil y)
	      ((type list)
	       (mapcar (lambda (xx yy) (string-or xx yy))
		       x y))))
	  lofl))

(defun string-or-test ()
  (is (string-or "" "B" "A")
      "B")
  (is (string-or "")
      ""))

(defun superposition-test ()
  (is (superposition '(("" "") (12 "") ("" 20) (30 40)))
      '(12 20))
  (is (superposition '(("" "") (12 "")))
      '(12 "")))

(defun hsido-person-interrupt (p)
#|
離脱情報。
|#
  (list (if (interrupt-> p) "○" "---")))

(defun hsido-person-date (p)
  (superposition
   (mapcar (lambda (f)
	     (list (hsido-firstd f) (hsido-finald f)))
	   (file-> p))))

(defun hsido-person-hospital (p)
  (& list
     kensin::code->hospital
     hsido-hcode
     car
     file-> p))

(defun hsido-person-upload (p hash-or-nil)
  (list
   (if hash-or-nil
       (if (gethash (rnumber-> p) hash-or-nil)
	   "○" "×")
       "---")))

(defmethod output ((p PERSON) hash-or-nil (op t))
  (format op "~{~A~^,~}~%"
	  (append (hsido-person-core p)
		  (hsido-person-hospital p)
		  (hsido-person-sign p)
		  (hsido-person-interrupt p)
		  (hsido-person-date p)
		  (hsido-person-upload p hash-or-nil))))

(defmethod add ((p PERSON) (h HSIDO))
  ;; (assert (equal (name-> p) (hsido-name h))
  ;; 	  (p h)
  ;; 	  "hsido-name-difference-error ~A v. ~A"
  ;; 	  (name-> p) (hsido-name h))
  (setf (file-> p)	(cons h (file-> p))
	(sign-> p)	(xor (sign-> p) (hsido-sign h))
	(interrupt-> p) (or (interrupt-> p) (hsido-interrupt h)))
  p)

(defun upable? (object)
  (optima:match (sign-> object)
    ((LIST _ _ _ t)
     (if (interrupt-> object)
	 :interrupt
	 :final))
    (_ nil)))

(defun hsido-iterate (keyfn valfn)
  (iter (with hash = (make-hash-table :test #'equal))
	(for p :in (@read))
	(setf (gethash (funcall keyfn p) hash)
	      (funcall valfn p hash))
	(finally (return hash))))

(defun filehash ()
  ;; ファイル名で対ファイル管理をする。
  (hsido-iterate
   #'hsido-zipname
   (lambda (p hash)
     (cons p (gethash (hsido-zipname p) hash)))))

(defun mainhash ()
  ;; 利用券整理番号を基に対人管理をする。
  (hsido-iterate
   #'hsido-rnumber			; 利用券整理番号
   (lambda (p hash)
     (aif (gethash (hsido-rnumber p) hash)
	  ;; すでにハッシュに登録されていれば追加する。
	  (add it p)
	  ;; 登録されていなければ生成する。
	  (create p)))))

(defun all-people (mainhash)
  (iter (for (k v) :in-hashtable mainhash)
	(collect v)))

(defun upable-people (mainhash)
  (iter (for (k v) :in-hashtable mainhash)
	(if (upable? v) (collect v))))

(defun upable-files (mainhash)
  ())
;; (defparameter h (let ((ksetting::*year* 2013))
;; 		  (mainhash)))

;; (iter (for (k v) :in-hashtable h)
;;       (print (list (level-> v) (sign-> v))))

;; (iter (with hash = (mainhash))
;;       (for (k v) :in-hashtable (filehash))
;;       (collect
;; 	  (mapcar (lambda (hsido-structure)
;; 		    (upable? (gethash (hsido-rnumber hsido-structure) hash)))
;; 		  v)))

(defun test ()
  (call-with-output-file2 "test.csv"
    (lambda (op)
      (let* ((ksetting::*year* 2013)
	     (hash (kensin::r165-hash)))
	(mapc (lambda (p) (output p hash op))
	      (all-people (mainhash)))))
    :code :SJIS))
