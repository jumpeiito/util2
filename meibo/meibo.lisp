(defpackage #:meibo
  (:nicknames :mb)
  (:use :cl :util :kensin :iterate :cl-win32ole :excel)
  (:import-from #:optima   #:match)
  (:import-from #:cl-ppcre #:regex-replace-all)
  (:import-from #:sb-thread
		#:make-thread
		#:join-thread))

(in-package :mb)

(defvar *year*		ksetting::*year*)
(defvar *nendo-end*	(nendo-end *year*))
(defvar *file*		ksetting::*zenken-file*)
(defvar *zenken-csv*	#P"f:/zenken.csv")
(defvar *zenken-hash*	#P"f:/zenken.hash")
(defvar *dock*		#P"f:/util2/kserv/.dock")
(defvar *sc*		#P"f:/util2/kserv/.setcontract")

;; (A) 全件データCSVで把握可能なもの
;;    1. 特定健診用氏名一覧CSVから読み込み
;;    2. 全件データCSVで受診日が入っているものを除外
;;    3. ソート
;;       基本的に記号番号で並べかえるが、電話番号が一緒のものも同時に表示できるようにする。
;;       したがって、記号番号順に並んでいないものが一部存在する。
;; (B) 全件データCSVで把握不可能なもの
;;    1. 特定健診用氏名一覧CSVから読み込み
;;    2. 支部名簿などから、受診者の受診券整理番号を抽出
;;    3. ドック・集合契約名簿から、該当支部分の受診券整理番号を抽出
;;    4. 2.3.の受診券整理番号の分を除外
;;    5. ソート
;;       基準は上記といっしょ。

;; 電話番号が空欄のときに出力するかどうか
(defvar output-if-telnum-is-blanked nil)
;; 民医連各院所で共通メニューが受診できなさそうな地域に色をつける。基本
;; 的に市内支部では有効にしておけばよいが、市外支部で有効にすると、色付
;; きがほとんどになってしまって見にくいので無効にする。
(defvar address-out-area-colored t)

(defpackage :kanbo/dock
  (:nicknames :kbd)
  (:use :cl :util :kensin :iterate)
  (:import-from #:optima #:match))

(in-package :kbd)			; ドック情報のハッシュを作るパッケージ

(defparameter file ksetting::*dock-output-file*)

(defstruct d appointment hospital kgbg name birth number flag date)

(defun dgenerate (line)
  (line-binding
   (line d)
   (appointment hospital kgbg name birth number flag _ _ _ date)))

(defun hash ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in (csv-read-to-list file))
	(match line
	  ((LIST* "" _) (next-iteration))
	  (_
	   (let* ((obj (dgenerate line))
		  (num (d-number obj)))	; 受診券番号or個人番号
	     (setf (gethash (if (eq 11 (length num))
				num	; 受診券番号
				(format nil "~9,,,'0@A" num)) ; 個人番号
			    hash)
		   obj))))
	(finally (return hash))))

(defparameter hash (hash))

(defpackage :kanbo/sc
  (:nicknames :kbsc)
  (:use :cl :util :kensin :iterate))

(in-package :kbsc)			; 集合契約のハッシュを作るパッケージ

(defparameter file ksetting::*sc-output-file*)

(defun hash ()
  (let ((hash (make-hash-table :test #'equal))
	(year (write-to-string ksetting::*year2*)))
    (declare (ignore year))
    (util::csv-read-iter
     file
     (lambda (line)
       (optima:match line
	 ((LIST* jnumber _)
	  (setf (gethash jnumber hash) line)))))
    hash))

(defparameter hash (hash))

(defpackage :kanbo/shibu-kenshin
  (:nicknames :kbsk)
  (:use :cl :util :kensin :iterate)
  (:import-from #:cxml #:parse-file)
  (:import-from #:stp
		#:make-builder
		#:string-value)
  (:import-from #:xpath
		#:evaluate
		#:do-node-set)
  (:import-from #:hx #:find-attribute))

(in-package :kbsk)			; 支部健診の情報を集めたハッシュを作るパッケージ(宇治-工場保健会など)

(defun stp ()
  (parse-file "f:/util2/meibo/kanbo-shibu.xml" (make-builder)))

(defun key (node)
  (let1 year (find-attribute node "year")
    (format nil "~A~A"
	    (find-attribute (stp:parent node) "code") year)))

(defun kbfilter (node)
  (stp:filter-children
   (lambda (child)
     (and (typep child 'stp:element)
	  (equal "hospital" (stp:local-name child))))
   node))

(defun hash ()
  (let ((shash (make-hash-table :test #'equal)))
    (do-node-set (node (evaluate "/root/shibu/item" (stp)) shash)
      (setf (gethash (key node) shash)
	    (mapcar
	     (lambda (n)
	       (vector (find-attribute n "code")
		       (find-attribute n "nickname")
		       (string-value n)))
	     (kbfilter node))))))

(defvar hash (hash))

(in-package :mb)

(defstruct SHIBU pathname code name csv title clojure)

(defstruct (LINE
	     (:constructor line (jnum name furigana gender birthday year
				 k b hk ad postal telnum knumber ig1 id ig2)))
  jnum name furigana gender birthday year k b hk ad
  postal telnum knumber ig1 id ig2 zenken hnum
  2012hit 2011hit 2010hit)


(defun kill-blank-literally (string)
  (ppcre:regex-replace-all "[　 ]+" string ""))

(defun dock2? (hcode nendo jnumber)
  (aif (and ;; (not (string= way "簡易入力"))
	    (gethash hcode kensin::dock-hash))
       (if (member (write-to-string nendo) it :test #'equal)
	   ;; 2010年度以前は名簿がないため、受診券チェックは行わない。
	   (if (or (<= nendo 2010)
		   (gethash jnumber kbd::hash))
	       (take it 2)
	       nil)
	   nil)
       nil))

(defun kenshin? (hcode scode1 scode2 year)
  "支部コードと年度、医療機関コードから支部健診かどうかを判定する。"
  (let ((key1 (format nil "~A~A" scode1 year))
	(key2 (format nil "~A~A" scode2 year)))
    (or (find-if (lambda (v) (equal hcode (svref v 0))) (gethash key1 kbsk::hash))
	(find-if (lambda (v) (equal hcode (svref v 0))) (gethash key2 kbsk::hash)))))

(defun sc? (jnumber)
  (gethash jnumber kbsc::hash))

;; "2014/01/17" -> "0117"
(defun regular-date (d)
  (subseq (regex-replace-all "/" d "") 4))

(defmacro judge-format (date format &rest args)
  `(format nil ,(format nil "~~A-~A" format)
	   ;; (regex-replace-all "/" (regular-date ,date) "")
	   (regular-date ,date)
	   ,@args))

;; (judge-format date "~A-支部健診" (svref it 1))

;; 2012年度以前
;; (1) 年度・医療機関コード -> ドック判定
;; (2) 支部・医療機関コード -> 支部健診判定
;; (3) その他
(defun judge (hit)
  (match hit
    ;; 国保未加入者
    ((TYPE NULL) "")
    ((LIST _ date hcode hname _ _ bunkai _ _ shibu1 shibu2 jnumber)
     (if date
	 ;; ------------------------------
	 ;; 受診者
	 ;; ------------------------------
	 (let1 nendo (nendo-year date)
	   (acond
	    ;; ---------- 支部健診
	    ((kenshin? hcode shibu1 shibu2 nendo)
	     (format nil "~A(~A)支"
		     (regular-date date) (svref it 1)))
	    ;; ---------- 集合契約
	    ((sc? jnumber)
	     (judge-format date "その他"))
	    ;; ---------- ドック
	    ((dock2? hcode nendo jnumber)
	     (format nil "~A(~A)ド"
		     (regular-date date) (second it)))
	    ;; ---------- 事業所健診など
	    (t
	     (judge-format date "その他"))))
	 ;; ------------------------------
	 ;; 未受診者
	 ;; ------------------------------
	 ""))))

(defun trans-hit (id hash)
  (let ((key (gethash id hash)))
    (if key
	;; ----------------------------------------
	;; 文字列にして返す -> パースが面倒
	;; ----------------------------------------
	;; (format nil "~{~A~^,~}"
	;; 	(mapcar (lambda (n) (or (nth n key) ""))
	;; 		(iota :from 1 :to 4)))
	;; ----------------------------------------
	;; リストにして返す -> パースが面倒、サイズが大きい
	;; ----------------------------------------
	;; (mapcar (lambda (n) (or (nth n key) ""))
	;; 	(iota :from 1 :to 4))
	;; ----------------------------------------
	;; とりあえず日付の部分だけ返す→第1案
	;; ----------------------------------------
	;; (let1 date (strdt (or (second key) ""))
	;;   (if date
	;;       (format nil "'~2,'0d~2,'0d"
	;; 	      (date-month date) (date-day date))
	;;       ""))
	;; ----------------------------------------
	;; 日付以外に、院所・判定も入れる
	;; ----------------------------------------
	(judge key)
	"")))

(defun omit-address (address)
  "空白を消すと同時に、「京都府・京都市」の部分を削除する。
引数: 住所(string)
返値: 新住所(string)
例:   (omit-address \"京都市南区　　　西九条豊田町3\")
-> \"南区西九条豊田町3\""
  (regex-replace-alist
   (kill-blank-literally address)
   '(("京都府" . "")
     ("京都市" . ""))))

(defun omit-kigo (kigo bango)
  "記号番号を省略した形式で返す。
引数: 記号(string) 番号(string)
返値: 記号番号(string)
例:   (omit-kigo \"建１３法８５\" \"５０４３６\")
-> \"法50436\""
  (ppcre:register-groups-bind (k)
      ("建..(.).." kigo)
    (format nil "~A~5,,,'0@A"
	    k (to-hankaku bango))))

(defun omit-year (year)
  (to-hankaku
   (ppcre:regex-replace-all "歳" year "")))

(defun omit-telnum (telnum)
  (ppcre:regex-replace-all "^075-" telnum ""))

(defun make-line (list zhash 2010hash 2011hash 2012hash)
  (let1 obj (apply #'line list)
    (with-slots (id ad jnum zenken hnum id 2010hit 2011hit 2012hit hk k b year telnum) obj
      (setq ad		(omit-address ad)
	    id		(format nil "~9,,,'0@A" id)
	    zenken	(gethash jnum zhash)
	    hnum	(read-from-string
			 (zenken::zenken-保険証番号 zenken))
	    2010hit	(trans-hit id 2010hash)
	    2011hit     (trans-hit id 2011hash)
	    2012hit     (trans-hit id 2012hash)
	    hk		(string-take hk 1)
	    k		(omit-kigo k b)
	    year	(omit-year year)
	    telnum	(omit-telnum telnum)))
    obj))

(defun line-bunkai (line)
  (or (zenken::zenken-分会名 (line-zenken line))
      ""))

(defun line-bunkai-code (line)
  (zenken::zenken-分会 (line-zenken line)))

(defun filename-parse (filename)
  (let ((f (pathname-name
	    (make-pathname :defaults filename))))
    (cl-ppcre:register-groups-bind
    	(code shibu)
    	("特定健診用氏名一覧(\\d{2})(.)_\\d{8}" f)
      (values (read-from-string code)
    	      (kensin::long-shibu code)))))

(defun clojure-sort-key (line)
  (with-slots (zenken) line
    (with-slots (zenken::支部
		 zenken::分会
		 zenken::班
		 zenken::保険証番号
		 zenken::行) zenken
      ;;----------------------------------------
      ;; 2014/01/30 変更
      ;; 支部・分会が同じならば、保険証番号の順番に並べるよう変更
      ;;----------------------------------------
      ;; (format nil "~A~A~A~A~A"
      ;; 	      zenken::支部 zenken::分会 zenken::班 zenken::保険証番号 zenken::行)
      (format nil "~A~A~A~A"
       	      zenken::支部 zenken::分会 zenken::保険証番号 zenken::行))))

(defun list-output-sort-key (list)
  ;; データの先頭、すなわち世帯主を抜きだす。
  (optima:match list
    ;; (電話番号 データ1 データ2 ..)
    ((LIST* _ head _)
     (clojure-sort-key head))))

(defun make-shibu-reader-list (hash)
  (sort (alexandria:hash-table-alist hash)
	(lambda (x y)
	  ;; 世帯主の支部・分会・班・保険証番号の順番で並びかえる。
	  (string< (list-output-sort-key x)
		   (list-output-sort-key y)))))

;;; 電話番号ごとにまとめるため。
(def-clojure shibu-reader ()
  ((hash	(make-hash-table :test #'equal)))
  ;; 行を読み込む
  (:add		(line)
		(let1 keyword (line-telnum line)
		  ;; 同一の電話番号をくくるために、ハッシュを作る。
		  (setf (gethash keyword hash)
			(aif (gethash keyword hash)
			     (sort (copy-list (cons line it))
				   (lambda (x y)
				     ;; 支部→分会→班→保険証番号→行
				     ;; をキーにして並びかえる。
				     (string< (clojure-sort-key x)
					      (clojure-sort-key y))))
			     (list line)))))
  (:list	()
		;; make-shibu-reader-list は、 世帯主の「支部→分会→班
		;; →保険証番号」で並べかえられたリスト。対象者は、該当
		;; 支部の40歳以上の、未受診である被保険者。
		(iter (for (telnum . data) :in (make-shibu-reader-list hash))
		      ;; グローバル変数output-if-telnum-is-blankedがTで
		      ;; あれば、電話番号が入っていなくても表に出力する。
		      ;; NILであれば、電話番号がなければ出力しない。
		      (if (if output-if-telnum-is-blanked
			      t
			      (ppcre:scan "^[0-9-]+$" telnum))
			  (appending data)))))

(defun target? (obj)
  (and obj (zenken::target? obj)
       (not (zenken::zenken-受診日 obj))
       (not (zenken::zenken-発行日 obj))))

(defun make-furigana-hash (list)
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in list)
	(optima:match line
	  ((LIST* _ _ furi _ _ _ k b hk _)
	   (if (equal hk "本人")
	       (setf (gethash (format nil "~A~A" k b) hash)
		     (car (ppcre:split " " furi))))))
	(finally (return hash))))

(defun furigana (line hash)
  (optima:match line
    ((LIST* _ _ furi _ _ _ k b _)
     (if (ppcre:scan "^ +$" furi)
  	 (setf (nth 2 line)
  	       (gethash (format nil "~A~A" k b) hash)))
     line)))

;; ファイルを用意して、例外的に未受診者から除外する。この除外ファイルは、
;; 受診券整理番号だけが入っている行が並んでいる。
(defun exception (filename)
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in-file filename :using #'read-line)
	;; 改行コードに対応するため。Windowsの改行コードCRLFからLFに変更。
	(setf (gethash (ppcre:regex-replace-all "" line "") hash)
	      1)
	(finally (return hash))))

(defun make-hash (file fn)
  (iter (with hash = (make-hash-table :test #'equal))
	(for line :in-csv file :code :UTF-8)
	(setf (gethash (funcall fn line) hash) 1)
	(finally (return hash))))

(defun dock ()
  (make-hash ksetting::*dock-output-file* #'sixth))

(defun sc ()
  (make-hash ksetting::*sc-output-file* #'first))

(defun %toCSV (list &optional exception)
  (iter (with hash     = (cl-store:restore #P"f:/zenken.hash"))
	(with 2012hash = (cl-store:restore "f:/20130628/kanbo/2012.hash"))
	(with 2011hash = (cl-store:restore "f:/20130628/kanbo/2011.hash"))
	(with 2010hash = (cl-store:restore "f:/20130628/kanbo/2010.hash"))
	(with dock     = (dock))
	(with sc       = (sc))
	(with furihash = (make-furigana-hash list))
	(for line :in list)
	(for jnum      = (car line))
	(for obj       = (gethash jnum hash))
	;; 関数target?は「受診していない」かつ「受診券を発行していない」
	;; ときにTとなる。
	(if (and (target? obj)
		 ;; 除外ファイルが指定されている場合は、除外ファイルに
		 ;; 受診券番号がないことが必須。
		 (if exception
		     (not (gethash jnum exception))
		     ;; 除外ファイルが指定されていない場合は、無条件に
		     ;; 通す。
		     t)
		 ;; ドックを受診していない。
		 (not (gethash jnum dock))
		 ;; 集合契約を受診していない。
		 (not (gethash jnum sc)))
	    (collect (make-line (furigana line furihash)
				hash 2010hash 2011hash 2012hash)))))

;; ファイル中の行を読みこんで、ハッシュを作る。
;; 最終的に、ハッシュを含んだクロージャを返す。
(defun create-shibu-function (csv)
  (let1 f (shibu-reader)
    (dolist (line csv) (funcall f :add line))
    f))

(defun create-shibu (pathname &optional exception)
  (let1 obj (make-shibu :pathname pathname)
    (let1 init (csv-read-to-list pathname :code :SJIS)
      (multiple-value-bind (vcode vname)
	  (filename-parse pathname)
	(with-slots (pathname code name csv title clojure) obj
	  (setq code	vcode
		name	vname
		csv	(%toCSV (cdr init) exception)
		title	(car init)
		clojure	(create-shibu-function csv)))))
    obj))

(defgeneric expose (s))
(defgeneric expose-meibo (s))

(defmethod expose ((line LINE))
  (with-slots (name furigana gender birthday year k b hk ad telnum
		    2010hit 2011hit 2012hit) line
    (list (line-bunkai line)
	  k name furigana telnum gender hk year ad
	  2010hit 2011hit 2012hit)))

(defmethod expose ((shibu SHIBU))
  (with-slots (clojure) shibu
    (iter (for line :in (funcall clojure :list))
	  (collect (expose line)))))

;----------------------------------------------------------------------------------------------------
; Excel 処理
;----------------------------------------------------------------------------------------------------
(defparameter width #(9 7.5 18 13 13 3 3 4 38 14 14 14 8 30))
(defparameter calign '(0 1 5 6 7 9 10 11))
(defparameter rightheader
  ;;----------------------------------------
  ;; 第1案からの変更 → 事務長より「受診しない理由のみでいい」とのこと
  ;;----------------------------------------
  ;; (format nil "~A~A~A~A"
  ;; 	  "&9&\"Palatino Linotype\""
  ;; 	  "対応コード…(10)受診する(種別不明) (11)支部健診受診 (12)共通メニュー受診 (13)特定健診受診"
  ;; 	  "(20)受診しない(理由不明) (21)多忙のため (22)職場の健診を受ける(た)ため (23)治療中 (24)不便 (25)無理解"
  ;; 	  "(31)不在 (32)ルス電")
  (format nil "~A~A~A"
  	  "&9&\"Palatino Linotype\""
	  (format nil "~{~A~^ ~}"
		  (mapcar-with-index
		   (lambda (c sym) (format nil "(~d) ~A" (1+ c) sym))
		   '(理由不明
		     多忙のため
		     職場の健診を受ける〔た〕ため
		     治療中
		     場所・時間・制度が不便である
		     健康面に自信がある
		     特定健診制度を理解していない〔受けなくてもよいと思っていたなど〕))
		  " ")
  	  "(A)不在 (B)ルス電"))
(defvar centerfooter "&18&\"Palatino Linotype\"&P/&N")
(defparameter page-row-size 49)
(defparameter hitdata-start-column 10)

(defun %PutData (sheet thread)
  (decide-range-value
   sheet `(("分会" "記号番号" "氏名" "フリガナ" "電話" "性別" "区分"
		   "年齢" "住所" "2010年" "2011年" "2012年"
		   "対応コード" "備考(要望・不満など具体的に)")))
  (decide-range-value
   sheet (sb-thread:join-thread thread)
   :start-row 2))

(defun row-for-erase (thread)
  (iter (generating line :in (sb-thread:join-thread thread))
	(for row :upfrom 2)
	(if (equal (nth 4 line) (nth 4 (next line)))
	    (collect row))))

(defun %row-for-merge (thread)
  (iter (with ary = (make-array '(5000) :initial-element 0))
	(for row :in (util::compact-str (row-for-erase thread)))
	(optima:match row
	  ((type ATOM)
	   (aref-1+ ary row))
	  ((LIST start end)
	   (iter (for i :from (1+ start) :to end)
		 (aref-1+ ary i))))
	(finally (return ary))))

(defun on-merge? (num ary)
  (eq 1 (svref ary num)))

(defun seek-not-on-merge (num ary)
  (iter (for i :downfrom num)
	(cond
	  ((eq i 0)
	   (leave nil))
	  ((eq 0 (svref ary i))
	   (leave i))
	  (t
	   (next-iteration)))))

(defun pagebreak (thread)
  (let* ((data   (sb-thread:join-thread thread))
	 (array  (%row-for-merge thread))
	 (length (length data)))
    (labels ((inner (counter r)
	       (if (> counter length)
		   (reverse r)
		   (let ((not-on-merge (seek-not-on-merge (+ page-row-size counter) array)))
		     (inner not-on-merge (cons not-on-merge r))))))
      (inner 2 nil))))

(defun row-for-merge (thread)
  (iter (for row :in (util::compact-str (row-for-erase thread)))
	(appending
	 (optima:match row
	   ((type ATOM)
	    (list (format nil "E~A:E~A" (1- row) row)
		  (format nil "M~A:M~A" (1- row) row)
		  (format nil "N~A:N~A" (1- row) row)))
	   ((LIST start end)
	    (list (format nil "E~A:E~A" (1- start) end)
		  (format nil "M~A:M~A" (1- start) end)
		  (format nil "N~A:N~A" (1- start) end)))))))

(defun line-erase (sheet thread)
  (iter (for row :in (row-for-erase thread))
	(setf (slot-value (ole sheet :range (format nil "A~A:I~:*~A" row) :borders 8)
			  :LineStyle)
	      nil)))

(defun line-merge (sheet thread)
  (iter (for range :in (row-for-merge thread))
	(format t "~A merged~%" range)
	(setf (slot-value (ole sheet :range range) :MergeCells)
	      t)))

;;----------------------------------------
;; 20140131 改ページの動作がおかしいので修正
;;----------------------------------------
;; 方針
;; 1 2行目からpage-row-size分だけとっていく。
;; 2 (+ 2 page-row-size)行目と次の行の電話番号が違えば次の行、
;;   つまり、(+ 3 page-row-size)行目を返す。
;; 3 同じであれば、バックトラックを行う。つまり、前の行にさかのぼって、
;;   次の行と電話番号が違う行を探し、その次の行を返す。
;; 4 以下同じ
(defun pagebreak2 (thread)
  (let* ((val (mapcar (lambda (l) (nth 4 l)) (join-thread thread)))
	 (prs page-row-size))
    ;; (labels ((in (start r)
    ;; 	       (if (<= start len)
    ;; 		   (progn
    ;; 		     (while (string= (nth (+ start prs) val) (nth (+ start prs 1) val))
    ;; 		       (setq start (1- start)))
    ;; 		     (in (+ start prs 1) (cons (+ start 1) r)))
    ;; 		   (reverse r))))
    ;;   (in (+ 2 prs) nil))
    (labels ((in (subl c r)
	       (if (or (null subl)
		       (< (length subl) prs))
		   (reverse r)
		   (progn
		     (let ((p prs))
		       (while (string= (nth p subl) (nth (1+ p) subl))
			 (print "backtracked")
			 (print (nth p subl))
			 (print (nth (1+ p) subl))
			 (setq p (1- p)))
		       (in (drop subl (1+ p)) (+ 1 p c) (cons (+ 1 p c) r)))))))
      (in val 2 nil))))
;;

(defun shrink-to-fit (sheet lastrow)
  (setf (slot-value (ole sheet :range (format nil "I2:I~A" lastrow))
		    :ShrinkToFit)
	t)
  (setf (slot-value (ole sheet :range (format nil "J2:L~A" lastrow))
		    :ShrinkToFit)
	t)
  (setf (slot-value (ole sheet :range "A1:M1")
		    :ShrinkToFit)
	t))

(defmacro page-setup (sheet &rest args)
  `(progn
     ,@(mapcar
	(lambda (l)
	  (optima:match l
	    ((LIST sym value)
	     `(setf (slot-value (ole ,sheet :PageSetup) ,sym) ,value))))
      (group args 2))))

(defun xls-border (sheet lastrow)
  (setf (slot-value (ole sheet :range (format nil "A1:O~A" lastrow) :Borders)
		    :LineStyle)
	1)
  (setf (slot-value (ole sheet :range (format nil "A1:O~A" lastrow) :Borders)
		    :Weight)
	1))

(defun rightfooter (shibu)
  (format nil "&\"HGP明朝B\"&28~A"
	  (shibu-name shibu)))

(defun center-align (sheet)
  (iter (for col :in calign)
	(for c = (excel::number-to-col (1+ col)))
	(excel::set-alignment sheet c
			      :HorizontalAlignment
			      excel::xlcenter))
  (excel::set-alignment sheet (:a 1) (:n 1)
			:HorizontalAlignment
			excel::xlcenter))

;; 色づけをするために、3年度分のヒットデータを抜き出す。
;; 支部健診とドックとそれ以外で分ける。
(defun hitdata-extract (shibu pred)
  "色づけをするために、3年度分のヒットデータを抜き出す。
pred: カウンタ 要素の2引数をとる関数"
  (iter (for line :in (expose shibu))
	(for row :upfrom 2)
	(for l = (take-right line 3))
	(appending (mapcar-with-index
		    (lambda (count el)
		      ;; あとで拡張性が必要になる場合があるかもしれないので、
		      ;; 関数として曖昧に定義しておく。
		      (if (funcall pred count el)
			  (format nil "~A~A" (excel::number-to-col (+ hitdata-start-column count)) row)
			  nil))
		    l)
		   :into pot)
	(finally (return (compact pot)))))

(defun hitdata-scan (shibu regexp)
  (hitdata-extract shibu
		   (lambda (count el)
		     (declare (ignorable count))
		     (ppcre:scan regexp el))))

;; 支部健診の場合
(defun hitdata-shibu (shibu)
  (hitdata-scan shibu "支$"))

;; 人間ドックの場合
(defun hitdata-dock (shibu)
  (hitdata-scan shibu "ド$"))

;; それ以外の場合
(defun hitdata-other (shibu)
  (hitdata-scan shibu "その他"))

(defun hitdata (shibu)
  (let* ((shb   (make-thread (lambda () (hitdata-shibu shibu))))
	 (dck   (make-thread (lambda () (hitdata-dock shibu))))
	 (other (make-thread (lambda () (hitdata-other shibu)))))
    (values (join-thread shb)
	    (join-thread dck)
	    (join-thread other))))

(defstruct hitdata-color font-color bg-color border bold)
(defparameter shibu-color
  (make-hitdata-color :font-color excel::xlblack
		      :bg-color   excel::xlgray25
		      :bold       t))
(defparameter dock-color
  (make-hitdata-color :font-color excel::xlwhite
		      :bg-color   excel::xlgray50
		      :bold       t))

(defun hitdata-coloring-core (sheet range hdc)
  (let ((font (hitdata-color-font-color hdc))
	(bg   (hitdata-color-bg-color hdc))
	(bold (hitdata-color-bold hdc)))
    (iter (for cell :in range)
	  (set-colorindex sheet cell :interior bg)
	  (set-colorindex sheet cell :font font)
	  (if bold
	      (setf (slot-value (ole sheet :range cell :font) :bold) t)))))

(defun hitdata-coloring (sheet shibu)
  (multiple-value-bind (s d o) (hitdata shibu)
    (hitdata-coloring-core sheet s shibu-color)
    (hitdata-coloring-core sheet d dock-color)
    ;; (hitdata-coloring sheet o)
    ))

;; 色づけをするために、3年度分のヒットデータを抜き出す。
;; 例えばセルの番号(A1、J102)のリストで返す。
;; (defun hitdata (shibu)
;;   (iter (for line :in (expose shibu))
;; 	(for row :upfrom 2)
;; 	(for l = (take-right line 3))
;; 	(appending (mapcar-with-index
;; 		    (lambda (count el)
;; 		      (if (string-null el)
;; 			  nil
;; 			  ;-------------------------------------------定数値
;; 			  (format nil "~A~A" (excel::number-to-col (+ 10 count)) row)))
;; 		    l)
;; 		   :into pot)
;; 	(finally (return (compact pot)))))

;; (defun address-in-area? (ad)
;;   ;--------------------------------------------------定数値
;;   (or (ppcre:scan "^([北南]|[上中下右左西]京|東山|伏見|山科)区" ad)
;;       (ppcre:scan "^(宇治|城陽|綾部|向日|長岡京)市" ad)
;;       (ppcre:scan "^久世郡久御山町" ad)))

;; 民医連各院所で共通メニューを受けられなさそうな地域を抜き出す。
;; (defun address-data (shibu)
;;   (iter (for line :in (expose shibu))
;; 	(for row :upfrom 2)
;; 	(if (address-in-area? (nth 8 line))
;; 	    (next-iteration)
;; 	    (collect (format nil "I~A" row)))))

;; (defun meibo-coloring (sheet cell)
;;   ;; (set-colorindex sheet cell :Interior excel::xlgray50)
;;   ;; (set-colorindex sheet cell :Font excel::xlwhite)
;;   (set-colorindex sheet cell :Interior excel::xlgray25))

;; (defun hitdata-color (sheet shibu)
;;   (iter (for cell :in (hitdata shibu))
;; 	(meibo-coloring sheet cell)))

(defun address-color (sheet shibu)
  (iter (for cell :in (address-data shibu))
	(meibo-coloring sheet cell)))

(defun solve-assert (shibu-code function-name)
  (let ((shibu-code-list
	 (mapcar (compose #'read-from-string #'car) kensin::long-shibu-alist)))
    (assert (member shibu-code shibu-code-list :test #'eq)
	    nil
	    (format nil "Invalid shibu-code in '~A'" function-name))))

(defun solve-pathname (shibu-code)
  (solve-assert shibu-code "solve-pathname")
  (find-if (lambda (p)
	     (ppcre:scan (format nil "特定健診用氏名一覧~A" shibu-code)
			 (namestring p)))
	   (directory-list #P"f:/util2/meibo/" :type "csv")))

(defun solve-exception (shibu-code)
  (solve-assert shibu-code "solve-exception")
  (aif (find-if (lambda (p)
		  (ppcre:scan (format nil "~A" shibu-code)
			      (namestring p)))
		(directory-list #P"f:/util2/meibo/exception/" :type "exception"))
       (exception it)
       nil))


(defun solve-savename (shibu)
  (make-pathname :defaults #P"f:/util2/meibo/"
		 :name     (format nil "電話かけ名簿~A~A_~A"
				   shibu
				   (long-shibu shibu)
				   (util::today-8))
		 :type     "xlsx"))

(defun xls (shibu);; (pathname &optional exception)
  (let* ((pathname  (solve-pathname shibu))
	 (exception (solve-exception shibu))
	 (shibu     (create-shibu pathname exception))
	 (%thread%  (sb-thread:make-thread
		     (lambda () (expose shibu)))))
    (with-excel (app :visible t :quit nil :debugger t)
      (let* ((book (ole app :Workbooks :Add))
  	     (sh   (ole book :Worksheets :item 1)))
	(%PutData sh %thread%)
	(let ((lr (lastrow sh :x 2 :y 1)))
	  (excel::set-colwidth sh width)
	  (xls-border sh lr)
	  (line-erase sh %thread%)
	  (shrink-to-fit sh lr)
	  (set-fontname sh (:a 1) (:n lr) "ＭＳ Ｐ明朝")
	  (center-align sh)
	  (setf (slot-value (ole sh :range (format nil "2:~A" lr))
			    :RowHeight)
		16)
	  (hitdata-coloring sh shibu)
	  ;; (if address-out-area-colored
	  ;;     (address-color sh shibu))
	  (page-setup sh
	  	      :PrintArea	(format nil "A1:N~A" lr)
	  	      :Orientation	2
	  	      :PaperSize	excel::xlA3 ;-- 20140130 変更
	  	      :PrintTitleRows	"$1:$1"
	  	      :FittoPagesTall	nil
	  	      :FittoPagesWide	1
	  	      :RightHeader	rightheader
	  	      :CenterFooter	centerfooter
	  	      :RightFooter	(rightfooter shibu))
 	  (ole sh :ResetAllPageBreaks)
	  ;;----------------------------------------
	  ;; 20140131 修正
	  ;;----------------------------------------
	  ;; (iter (for row :in (pagebreak %thread%))
	  ;; 	(print row)
	  ;; 	(setf (slot-value (ole sh :range (format nil "~A:~:*~A" row))
	  ;; 			  :PageBreak)
	  ;; 	      -4135))
	  (iter (for row :in (pagebreak2 %thread%))
	  	(setf (slot-value (ole sh :range (format nil "~A:~:*~A" row))
	  			  :PageBreak)
	  	      -4135))

	  (line-merge sh %thread%)
	  ;; (excel::save-book book (solve-savename shibu) :xlsx)
	  )))))

(defpackage #:meibo-otokuni
  (:nicknames #:moto)
  (:use #:cl #:util #:kensin #:iterate #:cl-win32ole #:excel)
  (:import-from #:optima #:match))

(in-package #:moto)

(defparameter file "f:/util2/meibo/2013乙訓健診用氏名一覧.xlsx")
(defparameter csvfile "f:/util2/meibo/特定健診用氏名一覧50乙_20140116.csv")

(defun extract ()
  (call-with-output-file2 "50otokuni.csv"
    (lambda (op)
      (with-excel (app :visible t :quit nil)
	(with-excel-book (app bk file :close nil)
	  (let* ((sh (ole bk :Worksheets :Item 1)))
	    (write (map 'vector
			(lambda (l) (vector (nth 7 l) (nth 15 l)))
			(ole sh :UsedRange :Value))
		   :stream op)))))))

(defun extract-number ()
  (call-with-input-file2 "50otokuni.csv"
    (lambda (in)
      (iter (for v :in-vector (read in))
	    (match v
	      ((OR (VECTOR NIL _)
		   (VECTOR _ "　　　　　　　　　　")
		   (VECTOR 0.0d0 _)
		   (VECTOR "∞" _)
		   (VECTOR "受診券整理番号" _))
	       (next-iteration))
	      ((VECTOR jnum _)
	       (collect (truncate jnum))))))))


(defun extract-number-from-dock ()
  (iter (for line :in-csv ksetting::*dock-output-file* :code :UTF-8)
	(for kgbg = (nth 2 line))
	(for jnum = (nth 5 line))
	(if (and (eq 2013 (kensin::jnum->nendo jnum))
		 (or (ppcre:scan "^50.+" kgbg)
		     (ppcre:scan "^8550.+" kgbg)))
	    (collect (read-from-string jnum)))))

(defun extract-number-from-sc ()
  (iter (for line :in-csv ksetting::*sc-output-file* :code :UTF-8)
	(for jnum  = (car line))
	(for shibu = (seventh line))
	(if (and (eq 2013 (kensin::jnum->nendo jnum))
		 (equal shibu "乙"))
	    (collect (read-from-string jnum)))))

(defun extract-append ()
  (append (extract-number)
	  (extract-number-from-dock)
	  (extract-number-from-sc)))

;; (defun make-exception-csv ()
;;   (call-with-output-file2 "20131216 乙訓除外.csv"
;;     (lambda (op)
;;       (iter (for line :in (extract-append))
;; 	    (format op "~A~%" line)))
;;     :code :SJIS))

(defun extract-hash ()
  (iter (with hash = (make-hash-table :test #'equal))
	(for num :in (extract-append))
	(setf (gethash num hash) 1)
	(finally (return hash))))

(defun make-exception-csv ()
  (call-with-output-file2 "20131216 乙訓除外.csv"
    (lambda (op)
      (let ((data (csv-read-to-list csvfile :code :SJIS))
	    (hash (extract-hash)))
	(format op "~{~A~^,~}~%" (car data))
	(iter (for line :in (cdr data))
	      (if (and (not (equal "0" (car line)))
	      	       (gethash (read-from-string (car line)) hash))
	      	  (format op "~{~A~^,~}~%" line)))))
    :code :SJIS))

(defpackage #:meibo/product
  (:nicknames #:pdct)
  (:use #:cl #:util #:kensin #:iterate #:cl-win32ole #:excel))

(in-package :pdct)

(defparameter dir #P"f:/util2/meibo/product/")

(defun 2- (n) (- n 2))

(defun pagebreak-collect (sheet lastrow)
  (iter (for page :from 1 :to (ole sheet :hpagebreaks :count))
	(collect (- (ole sheet :hpagebreaks page :location :row) 2))))

(defun telnum-collect (sheet lastrow)
  (mapcar #'car (value sheet (:e 2) (:e lastrow))))

(defun telnum-count (telmother numlist)
  (labels ((in (subtel subnum pre r)
	     (if (null subnum)
		 (reverse (cons (count-if #'identity subtel) r))
		 (let1 gap (- (car subnum) pre)
		   (in (drop subtel gap)
		       (cdr subnum)
		       (car subnum)
		       (cons (count-if #'identity (take subtel gap)) r))))))
    (in telmother numlist 0 nil)))

(defun investigate (app xls)
  (with-excel-book (app bk xls :close t)
    (let* ((sh    (ole bk :WorkSheets :Item 1))
	   (lr    (lastrow sh :y 1 :x 2))
	   (tel   (telnum-collect sh lr)))
      (iter (with name = (pathname-name xls))
	    (for page :in (telnum-count tel
					(pagebreak-collect sh lr)))
	    (for p :upfrom 1)
	    (format t "~A,~A枚目,~A~%" name p page)))))

(defun investigate-directory ()
  (with-excel (app :visible t :quit nil)
    (iter (for file :in (directory-list dir :type "xlsx"))
  	  (investigate app (namestring file)))))

(defstruct segundo telnum history sign row)

(defun create-segundo (line row)
  (make-segundo :telnum  (fifth line)
		:history (mapcar (lambda (n) (nth n line)) (list 9 10 11))
		:sign    (nth 12 line)
		:row     row))

(defun secondary-data-pre-process (sheet)
  (mapcar-with-index
   (lambda (counter el) (create-segundo el counter))
   (cdr (ole sheet :usedrange :value))
   :start 2))

(defun secondary-data (sheet)
  (let ((val (secondary-data-pre-process sheet)))
    (labels ((in (subl small r)
    	       (if (null subl)
    		   (reverse (cons (reverse small) r))
    		   (if (segundo-telnum (car subl))
		       (in (cdr subl) (list (car subl))
			   (if small
			       (cons (reverse small) r)
			       r))
		       (in (cdr subl) (cons (car subl) small) r)))))
      (in val nil nil))))

;; (defun secondary-history (2dl)
;;   (mapcar (lambda (l) (mapcar (lambda (n) (nth n l)) '(9 10 11)))
;; 	  2dl))

(defun secondary-sign (2dl)
  (segundo-sign (car 2dl)))

;; ;; 世帯のうち1人が3年間に1回でも受けていればT  (最も広い)
;; (defun secondary-condition1 (2dl)
;;   (not
;;    (any (lambda (l) (equal l '(nil nil nil)))
;; 	(secondary-history 2dl))))
(defun secondary-condition1 (2dl)
  (any (lambda (l) (not (equal l '(nil nil nil))))
       (mapcar #'segundo-history 2dl)))

;; ;; 世帯のうち1人が昨年度に受けていればT
(defun secondary-condition2 (2dl)
  (any #'identity
       (mapcar (lambda (l) (last1 (segundo-history l))) 2dl)))

(defun secondary-main-condition (2dl)
  (and (let1 sign (secondary-sign 2dl)
	 (or (string= "A" sign) (string= "B" sign)))
       (secondary-condition1 2dl)))

(defun secondary-rows (2dl)
  (mapcar #'segundo-row 2dl))

(defun secondary-removable-rows (2dl)
  (mapcan #'secondary-rows
	  (remove-if #'secondary-main-condition
		     2dl)))

(defun secondary-removable-rows-string (2dl)
  (mapcar
   (lambda (l)
     (optima:match l
       ((LIST x y)    (format nil "~A:~A" x y))
       (x (format nil "~A:~A" x x))))
   (secondary-removable-rows 2dl)))

;; (defun 2dl-with-row-number (2dl &key (start 1))
;;   (mapcar-with-index #'cons 2dl :start start))

;; f:/util2/meibo/product/sample.xlsx
(defun secondary (pathname)
  (with-excel (app :visible t :quit nil)
    (with-excel-book (app book pathname :close nil)
      (let* ((sh   (ole book :WorkSheets :Item 1))
	     (data (secondary-data sh)))
	(iter (for rows :in (reverse (secondary-removable-rows-string data)))
	      ;; (ole sh :range rows :select)
	      (ole sh :range rows :delete))
	(ole sh :cells 1 1 :select)))))

;; (mapcar-with-index)
(in-package :mb)
(defun test ()
  (let ((hash (cl-store:restore "f:/zenken.hash")))
    (util::csv-read-iter
     "f:/util2/meibo/特定健診用氏名一覧18右_20140116.csv"
     (lambda (line)
       (optima:match line
	 ((LIST* "受診券整理番号" _) :ignore)
	 ((LIST* "0" _) :ignore)
	 (_
	  ;; (print (gethash (car line) hash))
	  (let ((z (gethash (car line) hash)))
	    (if (and (zenken::zenken-発行日 z)
		     (not (zenken::zenken-受診日 z)))
		(print line))))))
     :code :SJIS)))

(defun test2 ()
  (let ((hash (cl-store:restore "f:/zenken.hash")))
    (util::csv-read-iter
     "f:/util2/meibo/TTK750.CSV"
     (lambda (line)
       (optima:match line
	 ((LIST* "受診券整理番号" _) :ignore)
	 ;; ((LIST* "0" _) :ignore)
	 (_
	  (let ((z (gethash (car line) hash)))
	    (if (and (zenken::zenken-発行日 z)
		     (not (zenken::zenken-受診日 z)))
		(print line))))))
     :code :SJIS)))

(defun test3 (filename)
  (let ((hash (make-hash-table :test #'equal)))
    (csv-read-iter
     filename
     (lambda (line)
       (optima:match line
	 ((LIST* "受診券整理番号" _) :ignore)
	 (_
	  (setf (gethash (format nil "~A~A" (nth 6 line) (nth 7 line)) hash)
		1))))
     :code :SJIS)
    hash))

(defun len (hash-table)
  (length (alexandria::hash-table-alist hash-table)))

(defun test4 ()
  (iter (for f :in (directory-list #P"f:/util2/meibo/archive/" :regexp "TTK.+"))
	(time (format t "~A : ~A~%" (pathname-name f) (len (test3 f))))))

(defun test5 (file app)
  (with-excel-book (app bk (namestring file) :close t :debugger t)
    (let ((val (ole bk :Worksheets :Item 1 :UsedRange :Value)))
      (call-with-output-file2
      	  (make-pathname :defaults file :type "csv")
      	(lambda (op)
      	  (iter (for line :in val)
      	  	(if (equal (fifth line) "電話")
      	  	    (next-iteration)
      	  	    (format op "~A,~{~A~^,~}~%"
			    (string-take (pathname-name file) 2)
			    (mapcar (lambda (o) (or o ""))
				    line)))))))))

(defun meibo-files ()
  (allf "y:/47伊東/2014年2月受診勧奨結果/" :type "xlsx" :regexp "^[0-9]"))
