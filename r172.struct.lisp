(in-package :R172c)

(defstruct r167 jnum date hp mlv hlv)

(defstruct (172data
	     (:constructor 172-gen (保険者番号 被保険者証記号 被保険者証番号 個人番号 データ管理番号１ 性別 生年月日 被保険者名カナ 被保険者名漢字 資格フラグ 除外フラグ 受診券整理番号 保健指導レベル 服薬再確認 健診メッセージID 健診メッセージ 利用券整理番号 初回面接実施日 途中終了 指導未完了 指導メッセージID 指導メッセージ))
	     ;; (:constructor )
	     )
  保険者番号 被保険者証記号 被保険者証番号 個人番号 データ管理番号１ 性別 生年月日 被保険者名カナ 被保険者名漢字 資格フラグ 除外フラグ 受診券整理番号 保健指導レベル 服薬再確認 健診メッセージID 健診メッセージ 利用券整理番号 初回面接実施日 途中終了 指導未完了 指導メッセージID 指導メッセージ 支部 zenken 受診日 医療機関 医療機関名 メタボレベル)


(defun create-172data (list zhash 167hash)
  (let1 obj (apply #'172-gen list)
    (with-slots (生年月日 支部 被保険者証記号 被保険者証番号 zenken 受診券整理番号
			  受診日 医療機関 医療機関名 メタボレベル) obj
      (setq 生年月日	(normal-date->string (strdt 生年月日))
	    支部	(cl-irregsexp:if-match-bind
			 ("建" (nendo (string 2)) (skanji (string)) (shibu (string 2)))
			 被保険者証記号
			 (if (equal shibu "８５")
			     (cl-irregsexp:if-match-bind
			      ((scode (string 2)) _) 被保険者証番号
			      (to-hankaku scode))
			     (to-hankaku shibu)))
	    zenken	(gethash 受診券整理番号 zhash))
      (aif (gethash 受診券整理番号 167hash)
	   (setq 受診日		(r167-date it)
		 医療機関	(r167-hp it)
		 医療機関名	(code->hospital 医療機関)
		 メタボレベル	(r167-mlv it)))
      obj)))

(defun get-year (172data)
  (with-slots (受診券整理番号 生年月日) 172data
    (jnum-how-old 生年月日 受診券整理番号)))

(defun h? (172data)
  (with-slots (受診券整理番号) 172data
    (string= (string-take-right 受診券整理番号 2) "01")))
