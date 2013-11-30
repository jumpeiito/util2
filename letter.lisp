(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util)
  (require :kensin))

(defpackage dock-letter
  (:nicknames :dl)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  )

(in-package :dl)

(defparameter year    ksetting::*year*)
(defparameter file    ksetting::*dock-output-file*)
(defparameter zipfile ksetting::*zip-parse-file*)

(defun ziphash ()
  (iter (for line :in (csv-read-to-list zipfile :code :SJiS))
	(shash line
	       :condition t
	       :key   #'third
	       :value #'identity)))

(defstruct (Person
	     (:constructor person (hospital kgbg name birth jnumber occur pay)))
  hospital kgbg name birth jnumber occur pay)

(defun maininit ()
  (iter (with hash = (ziphash))
	(for line :in (csv-read-to-list file))
	(optima:match line
	  ((LIST* _ _ _ _ _ _ _ _ _ _ _ _ "" _)
	   (next-iteration))
	  ((LIST* _ _ _ _ _ _ "" _)
	   (next-iteration))
	  ((LIST* _ hosp kgbg name birth jnum "1" _ _ _ occd _ pay _)
	   (let ((how-old (how-old birth (nendo-end year))))
	     (if (and (eq year (nendo-year occd))
		      (>= how-old 40) (< how-old 75))
		 (if (gethash jnum hash)
		     (next-iteration)
		     (collect (person hosp kgbg name birth jnum occd pay)))
		 (next-iteration)))))))

(defun occur-sort (list)
  (sort (copy-list list)
	(lambda (x y) (string< (person-occur x) (person-occur y)))))

(defun pay-sort (list)
  (sort (copy-list list)
	(lambda (x y) (string< (person-pay x) (person-pay y)))))

(defun all-hospitals (main)
  (uniq (mapcar #'person-hospital main)))

(defun search-hp (main hp)
  (occur-sort
   (remove-if-not
    (lambda (l) (equal hp (person-hospital l)))
    main)))

(in-package :cl-user)
