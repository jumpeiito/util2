(in-package :kcsv)

(defparameter duplicate-trash-box
  (cl-fad:pathname-as-directory
   (merge-pathnames topdir
		    "trash")))

(defun duplicate-hash ()
  (iter (for csv :in (mapcar #'create-csv
			     (directory-list topdir :type "csv")))
	(phash csv
	       :condition t
	       :key	  (lambda (s) (list (csv-hcode s)
					    (csv-occurd s)))
	       :value	  #'identity)))

(defun %duplicate-header-print (key)
  (util::stdout "~A	~A(~A)~%==================================================~%"
		(second key) (first key)
		(kensin:code->hospital (first key))))

(defun %duplicate-footer-print (list)
  (util::stdout "(~A) どのファイルも移動しない~%" (1+ (length list)))
  (util::stdout ">> "))

(defun remove-end (target-string)
  (ppcre:regex-replace-all "[
]+$" target-string ""))

(defun rename (s1 s2)
  (format *standard-output* "~A -> ~A" s1 s2)
  (rename-file s1 s2))

(defun read-input ()
  (read-from-string (remove-end (read-line))))

(defun input-correct? (input list)
  (and (numberp input)
       (>= input 1)
       (<= input (1+ (length list)))))

(defun %duplicate-read-and-execute (key list)
  (let* ((input (read-input)))
    (if (input-correct? input list)
	(if (eq input (1+ (length list)))
	    (util::stdout "何も移動しませんでした。~%")
	    (let1 path (csv-pathname (nth (1- input) list))
	      (rename path
		      (merge-pathnames duplicate-trash-box path))))
	(progn
	  (util::stdout "入力した番号が誤っています。")
	  (duplicate-operation key list)))))

(defmethod duplicate-print ((c csv) counter)
  (util::stdout "(~A) ~A: ~A人 ~{~A~^,~}~%"
		counter
		(pathname-name (csv-pathname c))
		(length (csv-body c))
		(mapcar #'line-name (take (csv-body c) 5))))

(defun duplicate-operation (key list)
  (%duplicate-header-print key)
  (iter (for l :in list)
	(for c :upfrom 1)
	(duplicate-print l c))
  (%duplicate-footer-print list)
  (%duplicate-read-and-execute key list))

(defun duplicate-check ()
  (make-directory duplicate-trash-box)
  (iter (for (k v) :in-hashtable (duplicate-hash))
	(if (> (length v) 1)
	    (duplicate-operation k v))))

(in-package :cl-user)
