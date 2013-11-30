(in-package :dock)

(defun alert-normal-sort (list)
  (multiple-sort list
		 (string< dock-健診機関)
		 (string< dock-受診日)))

(defun alert-print (list &optional op)
  (format op "~{~A~^,~}~%"
	  (list "健診機関" "受診日" "受診年度" "支払日" "保険番号"
		"整理番号" "氏名" "生年月日"))
  (iter (for line :in (alert-normal-sort list))
	(with-dock-slot line
	  (format (or op *standard-output*)
		  "~{~A~^,~}~%"
		  (list 健診機関 受診日 年度 支払日 保険番号 整理番号 氏名 生年月日)))))

;; (defparameter namelist (namelist))
(defun alert (&key (file nil))
  (iter (for name :in (namelist))
	(for old = (how-old (dock-生年月日 name)
			    (nendo-end (read-from-string ks::year))))
	(with-dock-slot name
	  (if (and (<= 40 old) (>= 75 old) (not ファイル)
		   (and 支払日 (not (string-null 支払日)))
		   (not 脳単独) (>= 年度 (read-from-string ks::year)))
	      (collect name :into pot)))
	(finally (return (if file
			     (call-with-output-file2 file (lambda (op) (alert-print pot op)) :code :SJIS)
			     (alert-print pot))))))

(in-package :cl-user)
