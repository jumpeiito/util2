(in-package :kensin)

(declaim (inline string-after))

(defparameter dname-file #P"f:/util2/dname/dname.xml")

(defun dname-xml ()
  (cxml:parse-file dname-file
		   (stp:make-builder)))

(defun dname-list ()
  (xpath:map-node-set->list
   #'identity
   (xpath:evaluate "/root/component/person" (dname-xml))))

(defun string-after (s1 s2)
  (if (string>= s1 s2) s1 s2))

(def-clojure dperson (stp hash)
  ((number   (stp:attribute-value stp "number"))
   (name     (stp:attribute-value stp "name"))
   (parent   (stp:parent stp))
   (pathname (stp:attribute-value parent "pathname"))
   (data172  (car (gethash number hash))))
  ;; (:check172 (hash) (aif (gethash number hash)
  ;; 			 (progn
  ;; 			   (setf data172 (car it))
  ;; 			   (if (or (not (172data-健診メッセージ (car it)))
  ;; 				   (string-not-null (172data-健診メッセージ (car it))))
  ;; 			       (values nil (car it))
  ;; 			       (values t (car it))))
  ;; 			 nil))
  )

(defun dname-collect ()
  (iter (with hash = (kensin::172-hash))
	(for line :in (dname-list))
	(for dp = (dperson line hash))
	(phash dp
	       :condition t
	       :key (lambda (f) (aif #[f :data172]
				     (kensin::172data-健診メッセージID it)
				     nil)))))

(defun dname-class ()
  (let1 hash (dname-collect)
    (values
     ;; 資格
     (mapcan (lambda (k) (gethash k hash)) '("MKCA01706I" "MKCA01707I"))
     ;; 評価項目欠損
     (mapcan (lambda (k) (gethash k hash))
	     '("MKCA01709E" "MKCA01723E" "MKCA01724W" "MKCA01757E"))
     ;; 問題なし
     (gethash "" hash)
     ;; なし
     (gethash nil hash))))
;; "MKCA01706I:4月1日時点で国保加入者であるが、年度途中で資格喪失をしたため報告対象外です。"
;; "MKCA01707I:4月1日時点で国保加入者でないため報告対象外です。"
;; "MKCA01709E:階層化に必要な健診項目に欠損があるため、報告対象外です。(内臓脂肪面積:腹囲(実測）)"
;; "MKCA01723E:報告項目が未実施のため報告対象外です。（ＢＭＩ）"
;; "MKCA01724W:健診結果に欠損があるため、評価対象者としてのみ取り扱います。（喫煙）"
;; "MKCA01757E:空腹時血糖、HbA1c(JDS値)のいずれか１項目以上実施していないため、報告対象外です。"
(multiple-value-bind (m a b c) (dname-class)
  (mapcar #[:data172] a))
