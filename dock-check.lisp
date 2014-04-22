(defpackage #:dock-check
  (:nicknames #:dc)
  (:use #:cl #:util #:kensin #:iterate))

(in-package #:dc)

(defparameter dir  #P"f:/util2/meibo/")
(defparameter dock ksetting::*dock-output-file*)

(defun files ()
  (directory-list dir :type "csv" :regexp "TTK750_.+"))

(defun file-to-jnums (file)
  (csv-read-filter-map
   file
   #'car
   (lambda (n) (not (string= n "受診券整理番号")))
   :code :SJIS))

(defun jnums-hash ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for file :in (files))
	(iter (for j :in (file-to-jnums file))
	      (setf (gethash j hash) 1))
	(finally (return hash))))

(defun dock-line ()
  (csv-read-filter-map
   dock
   ;; (lambda (n) (nth 5 n))
   #'identity
   (lambda (line)
     (let ((j (nth 5 line)))
       (and (eq 11 (length j))
	    (ppcre:scan "^13" j))))))

(defun check-jnums ()
  (iter (with hash = (jnums-hash))
	(for line :in (dock-line))
	(for j = (nth 5 line))
	(if (gethash j hash)
	    (print line))))
