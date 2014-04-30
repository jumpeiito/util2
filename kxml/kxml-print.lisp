(in-package :kxml)

(defparameter main-id-list
  '("9N001000000000001" "9N006000000000001" "9N011000000000001"
    "9N021000000000001" "9N016160100000001" "9N056000000000011"
    "9N056160400000049" "9N061000000000011" "9N061160800000049"
    "9N066000000000011" "9N066160800000049" "9A755000000000001"
    "9A752000000000001" "9A751000000000001" "9A765000000000001"
    "9A762000000000001" "9A761000000000001"
    "3F015000002327101" "3F015000002327201" "3F015000002399901"
    "3F070000002327101" "3F070000002327201" "3F070000002399901"
    "3F077000002327101" "3F077000002327201" "3F077000002399901"
    "3B035000002327201" "3B035000002399901" 
    "3B045000002327201" "3B045000002399901"
    "3B090000002327101" "3B090000002399901"
    "3C015000002327101" "3C020000002327101"
    "3D010000001926101" "3D010000002227101" "3D010000001927201"
    "3D010129901926101" "3D045000001906202" "3D045000001920402"
    "3D045000001927102" "3D045000001999902" "1A020000000191111"
    "1A020000000190111" "1A010000000191111" "1A010000000190111"
    "9N501000000000011"
    "9N506000000000011" "9N511000000000049" "9N516000000000049"
    "9N701000000000011" "9N706000000000011" "9N711000000000011"
    "9N716000000000011" "9N721000000000011" "9N726000000000011"
    "9N731000000000011" "9N736000000000011"))

(defparameter initial-column-list
  '("医療機関コード" "医療機関" "受診日" "整理番号" "記号" "番号" "氏名" "生年月日"))

(defparameter files
  '(#P"f:/zip/MAIN/2013/2614001234_00263129_201308230_1.zip"
    #P"f:/zip/MAIN/2013/2611800950_00263129_201306070_1.zip"
    #P"f:/zip/MAIN/2013/2610601342_00263129_201307100_1.zip"
    #P"f:/zip/MAIN/2013/2610601342_00263129_201305020_1.zip"
    #P"f:/zip/MAIN/2013/2610405348_00263129_201310071_1.zip"
    #P"f:/zip/MAIN/2013/2610301430_00263129_201309040_1.zip"
    #P"f:/zip/MAIN/2013/2610301430_00263129_201306070_1.zip"))

(defparameter kxml-print-function-hash
  (alexandria:alist-hash-table
   `(("整理番号"	. ,#'jnumber)
     ("記号"		. ,#'knumber)
     ("番号"		. ,#'bnumber)
     ("氏名"		. ,#'name)
     ("生年月日"	. ,#'birthday)
     ("医療機関コード"	. ,#'hospital-code)
     ("医療機関"	. ,#'hospital-name)
     ("受診日"		. ,#'occur-day))
   :test #'equal))

(defgeneric make-initial-column (arg list))
(defgeneric xml-parse-to-column (arg))
(defmethod make-initial-column ((xp XML-PARSER) list)
  (mapcar (lambda (f) (funcall (gethash f kxml-print-function-hash) xp))
	  list))

(defmethod xml-parse-to-column ((xp XML-PARSER))
  (iter (with gen = (make-initial-column xp initial-column-list))
	(with hash = (level1-hash xp))
	(for id :in main-id-list)
	(collect (gethash id hash "") :into pot)
	(finally (return (append gen pot)))))

(defun make-title1 ()
  (append (make-list (length initial-column-list)
		     :initial-element "")
	  main-id-list))

(defun make-title2 ()
  (append initial-column-list
	  (mapcar #'kcsv::code->title main-id-list)))

(defun make-main-list (zipfile)
  (append (list (make-title1))
	  (list (make-title2))
	  (mapcar #'xml-parse-to-column
		  (parse-zipfile zipfile))))

(defun zip-to-html (zipfile &key output)
  (let* ((main  (make-main-list zipfile))
	 (odir  (or output #P"./"))
	 (ofile (merge-pathnames
		 (make-pathname :name (pathname-name zipfile)
				:type "html")
		 odir)))
    (with-open-file (op ofile :direction :output :if-exists :supersede)
      (cl-who:with-html-output (i op)
    	(:html
    	 (:body
    	  (:table :frame "border" :rules "all"
    		  (loop
    		     :for k :from 0 :to (1- (length (car main)))
    		     :do  (cl-who:htm
    			   (:tr
    			    (loop
    			       :for l :from 0 :to (1- (length main))
    			       :for line = (nth l main)
    			       :do
    			       (if (remove-if #'string-null (nthcdr 2 (mapcar (lambda (x-l) (nth k x-l)) main)))
    				   (if (< l 2)
    				       (cl-who:htm
    					(:td :bgcolor "midnightblue"
    					     (:font :color "white"
    						    (cl-who:str (nth k line)))))
    				       (cl-who:htm
    					(:td (cl-who:str (nth k line)))))
    				   (print :skipped)))))))))))
    ))

