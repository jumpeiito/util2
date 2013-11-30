(in-package :uplog)

(defparameter topdir #P"g:/")

(defun pathname-date (pathname)
  (let1 basename (last1 (pathname-directory pathname))
    (cl-irregsexp:if-match-bind
     (_ (date (integer :length 8)))
     (the string basename)
     date)))

(defun uplog-initial ()
  (remove-if-not
   (lambda (p) (and (eq nil (pathname-name p))
		    (cl-irregsexp:if-match-bind
		     (_ (or "アップ" "up" "UP"))
		     (the string (last1 (pathname-directory p))))
		    (pathname-date p)))
   (directory-list topdir)))

(defun uplog-main ()
  (iter (for directory :in (sort2 (uplog-initial) > pathname-date))
	(appending (iter (for file :in-allf directory)
			 (collect (list directory (pathname-name file) (pathname-type file)))))))

(defun uplog-output ()
  (iter ;; (for directory :in (sort2 (uplog-initial) > pathname-date))
	;; (iter (for file :in-allf directory)
	;;       (format *standard-output* "~{~A,~A.~A~}~%"
	;; 	      (list directory (pathname-name file) (pathname-type file))))
	(for (dir name type) :in (uplog-main))
	(format *standard-output* "~A,~A.~A~%" dir name type)))

(defun uplog-search (search-keyword)
  (iter (for (dir name type) :in (uplog-main))
	(if (or (ppcre:scan search-keyword name)
		(ppcre:scan search-keyword type))
	    (collect (list dir name type)))))

(defun uplog-hash ()
  (iter (for line :in (uplog-main))
	(phash line :condition t
	       :key   #'second
	       :value (compose #'pathname-date #'first))))

(defun uplog (&optional key)
  (if key
      (uplog-search key)
      (uplog-output)))

(in-package :cl-user)
