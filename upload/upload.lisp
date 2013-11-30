(in-package :up)

(defparameter parse-file ksetting::*zip-parse-file*)
(defparameter upload-usb "g:/")

(define-condition upload-usb-doesnt-exist () ())

(defstruct zipfile main xmls type)

(defun upload-directory ()
  (remove-if-not
   (lambda (p) (ppcre:scan "_up" (namestring p)))
   (directory-list upload-usb)))

(defun allfiles (directory)
  (mapcar #'pathname-name
	  (allf directory :type "zip")))

(defun upload-files ()
  (mapcan #'allfiles
	  (upload-directory)))

(defun not-upload? (jnum hash)
  (eq :NOT-UPLOADED (kensin::172-uploaded? jnum hash)))

(defun usb-not-has-file? (zipname upload-files)
  (not (member zipname upload-files :test #'equal)))

(defun not-upload-file ()
  (iter (with hash = (kensin::172-hash))
	(for line :in-csv parse-file :code :SJIS)
	(optima:match line
	  ((LIST* zip _ jnum _)
	   (if (not-upload? jnum hash)
	       (collect zip :into pot))))
	(finally (return (uniq pot)))))

(defun new-directory ()
  (cl-fad:pathname-as-directory
   (merge-pathnames
    upload-usb (format nil "~A_up" (util::today-8)))))

(defun zip-directory ()
  (iter (for d :in ksetting::*zip-directory*)
	(if (cl-fad:file-exists-p d)
	    (leave d))
	(finally (error "zip directory not found"))))

(defun old-directory ()
  (make-pathname
   :defaults (format nil "~AMAIN/~A/" (zip-directory) ksetting::*year*)))

(defun mkdir ()
  (util::make-directory (new-directory)))

(defun copy ()
  (mkdir)
  (let ((upload (upload-files)))
    (if upload
	(iter (with old    = (old-directory))
	      (with new    = (new-directory))
	      (with upload = (upload-files))
	      (for file :in  (not-upload-file))
	      (for oldpath = (make-pathname :defaults old
					    :name file
					    :type "zip"))
	      (for newpath = (make-pathname :defaults new
					    :name file
					    :type "zip"))
	      (when (usb-not-has-file? file upload)
		(format t "~A ==> ~A~%" oldpath newpath)
		(cl-fad:copy-file oldpath newpath)))
	(format t "コピーするファイルがありません。~%"))))
