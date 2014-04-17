﻿(format t "hoge");; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   (require :cl-ppcre)
;;   (load-lib "iterate")
;;   (load-lib "util")
;;   (load-lib "list")
;;   (load-lib "hash"))
(in-package :kensin)

(defun filename-basename (pathname)
  (format nil "~A\.~A"
	  (pathname-name pathname)
	  (pathname-type pathname)))

(defun rengokai-file-p (pathname)
  (ppcre:scan "FKAC522.+csv" (filename-basename pathname)))

(defvar *dock-file*
  (or (cl-fad:file-exists-p "f:/util2/dock.csv")
      (cl-fad:file-exists-p "y:/47伊東/util2/dock.csv")))

(defparameter hospital-hash
  ;(the hash-table
  (push-hash-table
     #'car #'cdr
     '(("2610903946" . "城南診療所")
       ("2610803013" . "東山診療所")
       ("2614102230" . "大宅診療所")
       ("2610201358" . "上京病院")
       ("2613000625" . "新河端病院")
       ("2610403590" . "西七条診療所")
       ("2610500916" . "吉祥院病院")
       ("2620700092" . "京都府交通労働等災害救済事業団")
       ("2610601342" . "第二中央病院")
       ("2610406569" . "西七条診療所")
       ("2610606093" . "洛北診療所")
       ("2613300108" . "たんご協立診療所")
       ("2610307411" . "太子道診療所")
       ("2611801123" . "京都協立病院")
       ("2612701488" . "まいづる協立診療所")
       ("2611202348" . "あさくら診療所")
       ("2613100656" . "医誠会診療所")
       ("2613000625" . "新河端病院")
       ("2614101075" . "洛和会音羽病院")
       ("2610204741" . "上京診療所")
       ("2610604080" . "川端診療所")
       ("2610303667" . "京都工場保健会")
       ("2620700027" . "京都工場保健会宇治支所")
       ("2612800710" . "きづ川病院")
       ("2613200209" . "田辺中央病院")
       ("2619700053" . "社会保険京都病院")
       ("2610405124" . "康生会武田病院")
       ("2610405231" . "武田健診センター")
       ("2619600311" . "京都市立病院")
       ("2614001234" . "洛西シミズ病院")
       ("2610305993" . "大和健診センター")
       ("2619700038" . "京都第一赤十字病院")
       ("2619700012" . "京都第二赤十字病院")
       ("2613100987" . "向日回生病院")
       ("2619600154" . "公立山城総合医療センター")
       ("2614000095" . "桂病院")
       ("2610503118" . "同仁会クリニック")
       ("2610306454" . "御池クリニック")
       ("2612600938" . "京都ルネス病院")
       ("2613400288" . "明治国際医療大学附属病院")
       ("2610306868" . "シミズ四条大宮クリニック")
       ("2614102552" . "ラクト健診センター")
       ("2610308393" . "四条烏丸クリニック")
       ("2610301430" . "京都予防医学センター")
       ("2619700095" . "舞鶴共済病院")
       ("2610906766" . "金井病院")
       ("2619600212" . "福知山市民病院")
       ("2610902245" . "大島病院")
       ("2611800950" . "綾部市立病院")
       ("2619700129" . "済生会京都府病院")
       ("2610903045" . "蘇生会総合病院")
       ("2614002539" . "洛西ニュータウン病院")
       ("2619600063" . "公立南丹病院")
       ("2610200251" . "堀川病院")
       ("2610405348" . "西村診療所")
       ("2610402832" . "京都南病院")
       ("2610406627" . "新京都南病院")
       ("2613344"    . "洛北診療所"))))

(defparameter hospital-short-hash
  ;(the hash-table
  (push-hash-table
     #'car #'cdr
     '(("2610903946" . "城南")
       ("2610803013" . "東山")
       ("2614102230" . "大宅")
       ("2610201358" . "上京")
       ("2613000625" . "新河端")
       ("2610403590" . "西七条")
       ("2610500916" . "吉祥院")
       ("2620700092" . "城南")
       ("2610601342" . "第二中央")
       ("2610406569" . "西七条")
       ("2610606093" . "洛北")
       ("2613300108" . "たんご協立")
       ("2610307411" . "太子道")
       ("2611801123" . "京都協立")
       ("2612701488" . "まいづる協立")
       ("2611202348" . "あさくら")
       ("2611100484" . "久御山南")
       ("2613100656" . "医誠会")
       ("2613000625" . "新河端")
       ("2614101075" . "音羽")
       ("2610204741" . "上京")
       ("2610604080" . "川端")
       ("2610303667" . "工場")
       ("2620700027" . "工場宇治")
       ("2612800710" . "きづ川")
       ("2613200209" . "田辺中央")
       ("2619700053" . "社保")
       ("2610405124" . "武田")
       ("2610405231" . "武田")
       ("2619600311" . "市立")
       ("2614001234" . "洛西シミズ")
       ("2610305993" . "大和")
       ("2619700038" . "第一日赤")
       ("2619700012" . "第二日赤")
       ("2613100987" . "向日回生")
       ("2619600154" . "山城")
       ("2614000095" . "桂")
       ("2610503118" . "同仁会")
       ("2610306454" . "御池")
       ("2612600938" . "ルネス")
       ("2613400288" . "明治")
       ("2610306868" . "四条大宮")
       ("2614102552" . "ラクト")
       ("2610308393" . "四条烏丸")
       ("2610301430" . "予防医学")
       ("2619700095" . "舞鶴共済")
       ("2619700061" . "舞鶴日赤")
       ("2610906766" . "金井")
       ("2619600212" . "福知山市民")
       ("2610902245" . "大島")
       ("2611800950" . "綾部市立")
       ("2619700129" . "済生会")
       ("2610903045" . "蘇生会")
       ("2614002539" . "洛西ニュータウン")
       ("2619600063" . "南丹")
       ("2610200251" . "堀川")
       ("2610405348" . "西村")
       ("2610402832" . "京都南")
       ("2610406627" . "新京都南")
       ("2610307262" . "大澤")
       ("2613300132" . "丹後ふるさと")
       ("2613300017" . "丹後中央")
       ("0002613344" . "洛北"))))

(defun dock-hash-generator ()
  (iter (for line :in-csv *dock-file* :code :SJIS)
	(shash line :condition t :key #'car :value #'cdr)))

(defparameter dock-hash (dock-hash-generator))

(defgeneric dock? (hcode year))
(defmethod  dock? ((hcode String) (year String))
  (aif (gethash hcode dock-hash)
       (if (member year it :test #'equal)
	   (values (first it) (second it))
	   nil)
       nil))
(defmethod  dock? ((hcode String) (year Fixnum))
  (dock? hcode (write-to-string year)))

(defparameter hospital-shibu-hash-gen
  (push-hash-table
   #'car #'cdr
  '(("2610201358" . (("10" "北" "北") ("11" "上" "上京")))
    ("2610204741" . (("10" "北" "北") ("11" "上" "上京")))
    ("2610307411" . (("12" "中" "中京") ("18" "右" "右京") ("95" "電" "電気")))
    ("2610403590" . (("13" "下" "下京") ("90" "表" "表具")))
    ("2610406569" . (("13" "下" "下京") ("90" "表" "表具")))
    ("2610500916" . (("14" "南" "南")))
    ("2610601342" . (("15" "左" "左京")))
    ("2610604080" . (("15" "左" "左京")))
    ("2610606093" . (("15" "左" "左京")))
    ("2610803013" . (("16" "東" "東山")))
    ("2610903946" . (("19" "西" "西京") ("20" "伏" "伏見")
		     ("21" "醍" "醍醐") ("53" "亀" "亀岡")
		     ("54" "船" "船井") ("57" "福" "福知山")
		     ("59" "宮" "宮津") ("61" "相" "相楽")
		     ("62" "洛" "洛南") ("63" "綴" "綴喜八幡")
		     ("95" "電" "電気")))
    ("2611202348" . (("51" "宇" "宇治")))
    ("2611801123" . (("56" "綾" "綾部")))
    ("2612701488" . (("58" "舞" "舞鶴")))
    ("2613000625" . (("50" "乙" "乙訓")))
    ("2613100656" . (("50" "乙" "乙訓")))
    ("2613300108" . (("60" "奥" "奥丹後")))
    ("2613344" . (("15" "左" "左京")))
    ("2614101075" . (("95" "電" "電気")))
    ("2614102230" . (("17" "山" "山科")))
    ("2620700092" . (("19" "西" "西京") ("20" "伏" "伏見")
		     ("21" "醍" "醍醐") ("53" "亀" "亀岡")
		     ("54" "船" "船井") ("57" "福" "福知山")
		     ("59" "宮" "宮津") ("61" "相" "相楽")
		     ("62" "洛" "洛南") ("63" "綴" "綴喜八幡")
		     ("95" "電" "電気"))))))

(defparameter hospital-shibu-hash
  (let1 h (make-hash-table :test #'equal)
    (iter (for (k v) :in-hashtable hospital-shibu-hash-gen)
	  (setf (gethash k h) (car v)))
    h))

(defun shibu-kenshin? (hcode scode)
  (aif (gethash hcode hospital-shibu-hash)
       (member scode it :key #'car :test #'equal)
       nil))

(defun code->hospital (hcode &key (default ""))
  (aif (gethash hcode hospital-hash)
       (car it) default))

(defparameter kgbg-regexp
  (str+ "[0-9]"
	"[北上中下南左東山右西伏醍乙宇亀船綾福舞宮奥相洛綴法表電]"
	"-[0-9]{5}"))

(defun kensin-csv->body (csv)
  (drop (butlast csv) 2))

(defun rtoj (rnum)
  (cl-irregsexp:if-match-bind ((nendo (string 2)) (string) (other (string +)))
			      (the string rnum)
			      (the string (format nil "~A1~A" nendo other))))

(defun shibu-from-kgbg (kgbg &key (type nil))
  (if (ppcre:scan "\\d{7,}" kgbg)
      (ppcre:register-groups-bind
	  (kg bg) (".*(\\d{2})(\\d{5})$" kgbg)
	(shibu-from-kgbg2 kg bg :type type))
      (error 'simple-error)))

(defun shibu-from-kgbg2 (kg bg &key (type nil))
  "type assumed to be :long-kanji(\"綴喜\") :short-kanji(\"綴\")
:kanji(\"綴\") :number(63) :string(\"63\") :identity(\"63\") nil(\"63\")"
  (funcall
   (if type
       (case type
	 (:long-kanji	#'kensin::long-shibu)
	 (:short-kanji	#'kensin::short-shibu)
	 (:kanji		#'kensin::short-shibu)
	 (:number		#'read-from-string)
	 (:string		#'identity)
	 (:identity		#'identity))
       #'identity)
   (if (equal "85" kg)
       (string-take bg 2)
       kg)))

(defun kensin-year? (year)
  (and (>= year 40) (< year 75)))

(defun jnum->nendo (jnum)
  (cl-irregsexp:if-match-bind
   ((nendo (string 2)) _)
   (the simple-string jnum)
   (+ 2000 (read-from-string nendo))))

(defun jnum->nendo-end (jnum)
  (cl-irregsexp:if-match-bind
   ((nendo (string 2)) _)
   (the simple-string jnum)
   (nendo-end (+ 2000 (read-from-string nendo)))))

(defgeneric jnum-how-old (birth jnum))
(defmethod jnum-how-old ((b LOCAL-TIME:TIMESTAMP) (jnum STRING))
  (how-old b (jnum->nendo-end jnum)))
(defmethod jnum-how-old ((b DT:DATE-TIME) (jnum STRING))
  (how-old b (jnum->nendo-end jnum)))
(defmethod jnum-how-old ((b STRING) (jnum STRING))
  (jnum-how-old (strdt b) jnum))

(defun sex (string)
  (string-case string
    (("男" "男性" "M" "m" "male" "MALE")
     1)
    (("女" "女性" "F" "f" "female" "FEMALE")
     2)))

(defun jnum->sex (jnum hash)
  (sex (zenken::zenken-性別 (gethash jnum hash))))

(defun hk (jnumber)
  (if (ppcre:scan "1$" jnumber) 1 2))

(in-package :cl-user)
