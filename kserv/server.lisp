(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util)
  (require :kensin)
  (require :bordeaux-threads))


(defun main ()
  (kserv::start)
  (loop
     (sleep 1)
     (if (ppcre:scan "stop" (read))
	 (kserv::stop))))
