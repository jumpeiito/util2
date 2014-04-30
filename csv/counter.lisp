(in-package :kcsv)

(defun count-initial (func)
  (iter (for file :in-directory topdir :type "csv" :regexp "00263129_FKAC522_\\d{8}_\\d{3}$")
	(appending (funcall func (create-csv file)))))

(defmethod shibu ((c csv))
  "((201205 \"95\" 18) (201205 \"85\" 2))のように出力"
  (with-csv-slots c
    (mapcar
     (lambda (cons)
       (let1 date (strdt occurd)
	 (list (+ (* 100 (date-year date)) (date-month date))
	       (car cons) (cdr cons))))
     (alexandria:hash-table-alist
      (iter (for line :in body)
	    (chash line :key #'line-shibu))))))

(defun counter-shibu-hash ()
  (iter (for s :in (count-initial #'shibu))
	(shash s
	       :condition t
	       :key   (lambda (o) (butlast o))
	       :value #'last1)))

(defun counter-shibu-initial-list (nendo)
  (mapcar
   (lambda (cons)
     (append (list (car cons))
	     (mapcar
	      (lambda (m) (list m (car cons)))
	      (nendo-month-list nendo))))
   kensin::short-shibu-alist))

(defun counter-shibu (nendo)
  (let1 hash (counter-shibu-hash)
    (iter (for (shibu . data) :in (counter-shibu-initial-list nendo))
	  (collect (cons shibu
			 (mapcar
			  (lambda (key) (gethash key hash 0))
			  data))))))

(in-package :cl-user)
