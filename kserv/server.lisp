(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util)
  (require :kensin)
  (require :bordeaux-threads))

;; (save-lisp-and-die "server.exe" :executable t :toplevel 'main)
;; (defun server-start (&rest args)
;;   (declare (ignorable args))
;;   (kserv::start))

;; (defun server-stop (&rest args)
;;   (declare (ignorable args))
;;   (kserv::stop))

(defun main ()
  (kserv::start)
  (loop
     (sleep 1)
     (if (ppcre:scan "stop" (read))
	 (kserv::stop))))
