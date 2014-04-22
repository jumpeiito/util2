(in-package :hsido)

(defvar TOPDIR ksetting::*hsido-directory*)
(defvar xmldir (cl-fad:pathname-as-directory
		(merge-pathnames hs::topdir
				 "xml")))

(defun xml-contents (xmlname)
  (cxml:parse-file xmlname (stp:make-builder)))

(defmacro has-predicate (xmlname code)
  `(not
    (xpath:node-set-empty-p
     (kxml:xpath #'identity (xml-contents ,xmlname)
		 "<cs>" ,(format nil "code[@code='~A']" code)))))

(defun has-first?     (xmlname) (has-predicate xmlname "90030"))
(defun has-continual? (xmlname) (has-predicate xmlname "90040"))
(defun has-internal?  (xmlname) (has-predicate xmlname "90050"))
(defun has-final?     (xmlname) (has-predicate xmlname "90060"))
;; "f:/zip/MAIN/HSIDO/xml/g26103074112013120612000005.xml"
(defun get-interrupted-value (xmlname)
  (kxml:xpath #'xpath:string-value
	      (xml-contents xmlname)
	      "<cs>" "code[@code='90060']/.." "entry" "act" "entryRelationship"
	      "observation" "code[@code='1042000116']/.." "value" ("value")))
(defun interrupted?   (xmlname)
  (let ((val (get-interrupted-value xmlname)))
    (and (not (equal "0" val))
	 (not (equal "" val)))))

(defstruct (hsido
	     (:constructor hsidoj (zipname
				   xmlname
				   jnumber
				   kigo
				   bango
				   name
				   birth
				   hcode
				   hname
				   created
				   occurd
				   rnumber
				   level
				   stage
				   cource
				   firstd
				   performer
				   finald)))
  zipname xmlname jnumber kigo bango name birth hcode
  hname created occurd rnumber level stage cource firstd performer
  finald hit 172hit hmes kmes sign interrupt)

(defmacro @fork (form)
  `(sb-thread:make-thread
    (lambda ()
      ,form)))

(defun appropriate-number? (number &rest chars)
  (and (eq 11 (length number))
       (let ((target-char (aref number 2)))
	 (find-if (lambda (c) (char-equal c target-char))
		  chars))
       (print ksetting::*year*)
       (equal (mod ksetting::*year* 1000)
	      (read-from-string
	       (string-take number 2))
	      )))

(defun appropriate-jnumber? (jnumber)
  (appropriate-number? jnumber #\1))

(defun appropriate-rnumber? (rnumber)
  (appropriate-number? rnumber #\2 #\3))


(defun make-hsido (list 172hash)
  (let1 obj (apply #'hsidoj list)
    (with-slots (xmlname jnumber rnumber 172hit hmes sign kmes name interrupt level) obj
      (let* ((xml   (merge-pathnames xmldir xmlname))
	     (first (@fork (has-first? xml)))
	     (cont  (@fork (has-continual? xml)))
	     (inter (@fork (has-internal? xml)))
	     (final (@fork (has-final? xml)))
	     (itrp  (@fork (interrupted? xml))))
	(setf (aref jnumber 2) #\1)
	(setq 172hit  (gethash jnumber 172hash)
	      xmlname xml
	      sign    (mapcar (lambda (th) (sb-thread:join-thread th))
			      (list first cont inter final))
	      interrupt (sb-thread:join-thread itrp))
	;; 正常でない利用券整理番号・受診券番号が入っている場合、警告を出して修正する。
	(when (not (appropriate-jnumber? jnumber))
	  (warn "Assertion of inappropriate jnumber(~A: ~A) in 'make-hsido'"
		jnumber name)
	  (setf (aref jnumber 2) #\1))
	(when (not (appropriate-rnumber? rnumber))
	  (warn "Assertion of inappropriate rnumber(~A: ~A) in 'make-hsido'"
		rnumber name)
	  (handler-case
	      (setf (aref rnumber 2)
		    (if (string= level "動機づけ支援") #\3 #\2))
	    (sb-int:invalid-array-index-error (e)
	      (declare (ignorable e))
	      (setq rnumber ""))))
	obj))))
