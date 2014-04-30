(in-package :hx)

(defvar *xmldir* (cl-fad:pathname-as-directory
		  (merge-pathnames ksetting::*hsido-directory*
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
