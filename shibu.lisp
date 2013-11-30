;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require :alexandria)
;;   (load-lib "iterate")
;;   (load-lib "util")
;;   (load-lib "string")
;;   (load-with-os "test.fasl"))
(in-package :kensin)

(defvar short-shibu-alist
  '(("10" . "北") ("11" . "上") ("12" . "中") ("13" . "下") ("14" . "南") ("15" . "左")
    ("16" . "東") ("17" . "山") ("18" . "右") ("19" . "西") ("20" . "伏") ("21" . "醍")
    ("50" . "乙") ("51" . "宇") ("53" . "亀") ("54" . "船") ("56" . "綾") ("57" . "福")
    ("58" . "舞") ("59" . "宮") ("60" . "奥") ("61" . "相") ("62" . "洛") ("63" . "綴")
    ("85" . "法") ("90" . "表") ("95" . "電")))

(defvar short-shibu-hash
  (alexandria:alist-hash-table
   short-shibu-alist
   :test #'equal))

(defun %make-vice-cons-list% (alist)
  (mapcar (lambda (cons) (cons (cdr cons) (car cons)))
	  alist))

(defvar short-vice-shibu-alist
  (%make-vice-cons-list% short-shibu-alist))

(defvar short-vice-shibu-hash
  (alexandria:alist-hash-table
   short-vice-shibu-alist
   :test #'equal))

(defvar long-shibu-alist
  '(("10" . "北") ("11" . "上京") ("12" . "中京") ("13" . "下京") ("14" . "南") ("15" . "左京")
    ("16" . "東山") ("17" . "山科") ("18" . "右京") ("19" . "西京") ("20" . "伏見") ("21" . "醍醐")
    ("50" . "乙訓") ("51" . "宇治") ("53" . "亀岡") ("54" . "船井") ("56" . "綾部") ("57" . "福知山")
    ("58" . "舞鶴") ("59" . "宮津") ("60" . "奥丹後") ("61" . "相楽") ("62" . "洛南") ("63" . "綴喜八幡")
    ("85" . "法人") ("90" . "表具") ("95" . "電工")))

(defvar long-vice-shibu-alist
  (%make-vice-cons-list% long-shibu-alist))

(defvar long-shibu-hash
  (alexandria:alist-hash-table
   long-shibu-alist
   :test #'equal))

(defvar long-vice-shibu-hash
  (alexandria:alist-hash-table
   long-vice-shibu-alist
   :test #'equal))

(defun short-shibu (num)
  (let1 n (if (numberp num) (write-to-string num) num)
    (gethash n short-shibu-hash)))

(defun long-shibu (num)
  (let1 n (if (numberp num) (write-to-string num) num)
    (gethash n long-shibu-hash)))

(defun kgbg-gen (num)
  (let1 n (if (numberp num) (write-to-string num) num)
    (cond
      ((eq 8 (length n)) (str+ "1" n))
      ((eq 9 (length n)) n)
      (t (error "kgbg is 8 or 9 length string.")))))

(defun kgbg (num)
  (let1 n (kgbg-gen num)
    (str+ "建13" (short-shibu (subseq n 2 4)) (subseq n 2 4)
	  "-" (subseq n 4 9))))

(defun shibu (kg bg)
  (ppcre:register-groups-bind (shibu) ("建...(..)" kg)
    (let1 sh (to-hankaku shibu)
      (if (equal "85" sh)
	  (ppcre:register-groups-bind (b) ("(..)..." bg)
	    (to-hankaku b))
	  sh))))

;; "北-12054"と"奥-01054"を比較するなど
(defun shibu-compare-transform (str)
  (rxmatch-bind "([北上中下南左東山右西伏醍乙宇亀船綾福舞宮奥相洛綴法表電])(.*)"
      str
      (shibu bango)
    (str+ (gethash shibu short-vice-shibu-hash nil)
	  (ppcre:regex-replace-all "[-－]" bango ""))))

(defun shibu< (str1 str2)
  (string< (shibu-compare-transform str1)
	   (shibu-compare-transform str2)))

(defun shibu> (str1 str2)
  (string> (shibu-compare-transform str1)
	   (shibu-compare-transform str2)))

(defparameter shibu-all
'((:name "北" :number "10"
   :branch ((:name "柊野"	:number "01")
	    (:name "上賀茂"	:number "02")
	    (:name "大宮"	:number "03")
	    (:name "鷹峯"	:number "04")
	    (:name "紫竹"	:number "05")
	    (:name "待鳳"	:number "06")
	    (:name "鳳徳"	:number "07")
	    (:name "紫野"	:number "08")
	    (:name "金閣"	:number "09")
	    (:name "衣笠"	:number "10")
	    (:name "出雲寺"	:number "11")
	    (:name "点在"	:number "12")))
  (:name "上京" :number "11" 
   :branch ((:name "仁和" :number "02")
	    (:name "西陣" :number "03")
	    (:name "小川" :number "05")
	    (:name "点在" :number "06")
	    (:name "千本" :number "07")
	    (:name "堀川" :number "08")
	    (:name "室町" :number "09")
	    (:name "寺町" :number "10")))
  (:name "中京" :number "12" 
   :branch ((:name "朱1" :number "01")
	    (:name "朱2" :number "02")
	    (:name "朱3" :number "03")
	    (:name "朱4" :number "04")
	    (:name "朱6" :number "06")
	    (:name "朱7" :number "07")
	    (:name "朱8" :number "08")
	    (:name "東" :number "09")
	    (:name "烏丸" :number "10")))
  (:name "下京" :number "13" 
   :branch ((:name "七条1"	:number "01")
	    (:name "七条2"	:number "02")
	    (:name "七条3"	:number "03")
	    (:name "東"		:number "04")
	    (:name "西"		:number "05")
	    (:name "淳風"	:number "06")
	    (:name "点在"	:number "08")))
  (:name "南" :number "14" 
   :branch ((:name "陶化" :number "01")
	    (:name "東和" :number "02")
	    (:name "西九条" :number "03")
	    (:name "唐橋" :number "04")
	    (:name "吉祥院" :number "05")
	    (:name "上鳥羽" :number "06")
	    (:name "久世" :number "07")
	    (:name "祥豊" :number "08")
	    (:name "点在" :number "09")
	    (:name "祥栄" :number "10")
	    (:name "南大内" :number "13")))
  (:name "左京" :number "15" 
   :branch ((:name "大原" :number "01")
	    (:name "洛北" :number "02")
	    (:name "修学院" :number "04")
	    (:name "一乗寺" :number "05")
	    (:name "高野" :number "06")
	    (:name "下鴨" :number "07")
	    (:name "銀閣寺" :number "08")
	    (:name "北白川" :number "09")
	    (:name "出町" :number "10")
	    (:name "吉田" :number "11")
	    (:name "聖護院" :number "12")
	    (:name "川端" :number "13")
	    (:name "点在" :number "14")
	    (:name "岩倉北" :number "15")
	    (:name "岩倉南" :number "16")
	    (:name "松ヶ崎" :number "17")))
  (:name "東山" :number "16" 
   :branch ((:number "01" :name "鴨東")
	    (:number "03" :name "今熊野")
	    (:number "05" :name "点在")))
  (:name "山科" :number "17" 
   :branch ((:number "30" :name "北")
	    (:number "31" :name "花山")
	    (:number "32" :name "中央")
	    (:number "33" :name "音羽山")
	    (:number "34" :name "山階南")
	    (:number "35" :name "大塚")
	    (:number "36" :name "大宅")
	    (:number "37" :name "勧修")
	    (:number "38" :name "百々東")
	    (:number "39" :name "百々西")
	    (:number "40" :name "西野山")
	    (:number "41" :name "小野")))
  (:name "右京" :number "18" 
   :branch ((:number "01" :name "西院")
	    (:number "02" :name "山ノ内")
	    (:number "03" :name "安井")
	    (:number "04" :name "太秦東")
	    (:number "05" :name "常盤")
	    (:number "06" :name "嵯峨野")
	    (:number "07" :name "嵯峨")
	    (:number "08" :name "高雄")
	    (:number "09" :name "御室")
	    (:number "10" :name "鳴滝")
	    (:number "11" :name "花園")
	    (:number "12" :name "北嵯峨")
	    (:number "13" :name "太秦西")
	    (:number "14" :name "西梅津")
	    (:number "15" :name "東梅津")
	    (:number "16" :name "郡")
	    (:number "17" :name "西京極")
	    (:number "18" :name "18分会")
	    (:number "19" :name "嵐山")
	    (:number "20" :name "高瀬川")
	    (:number "21" :name "京北")))
  (:name "西京" :number "19" 
   :branch ((:number "01" :name "嵐山")
	    (:number "02" :name "松尾東")
	    (:number "03" :name "松尾西")
	    (:number "04" :name "松陽")
	    (:number "05" :name "上桂")
	    (:number "06" :name "桂")
	    (:number "08" :name "桂東")
	    (:number "09" :name "川岡")
	    (:number "10" :name "川岡東")
	    (:number "11" :name "樫原")
	    (:number "12" :name "大枝")
	    (:number "13" :name "洛西")
	    (:number "15" :name "大原野")
	    (:number "17" :name "境谷")
	    (:number "18" :name "中山")
	    (:number "19" :name "西山")
	    (:number "20" :name "桂川")
	    (:number "21" :name "新林")
	    (:number "22" :name "福西")
	    (:number "23" :name "桂坂")))
  (:name "伏見" :number "20" 
   :branch ((:number "01" :name "砂川")
	    (:number "02" :name "深草")
	    (:number "03" :name "藤の森")
	    (:number "04" :name "住吉")
	    (:number "05" :name "板橋")
	    (:number "06" :name "南浜")
	    (:number "07" :name "桃山")
	    (:number "11" :name "淀")
	    (:number "12" :name "納所")
	    (:number "14" :name "点在")
	    (:number "18" :name "向島東")
	    (:number "19" :name "向島西")
	    (:number "20" :name "向島NT")
	    (:number "31" :name "久我")
	    (:number "32" :name "羽束師")))
  (:name "醍醐" :number "21" 
   :branch ((:number "01" :name "石田")
	    (:number "02" :name "日野")
	    (:number "03" :name "小栗栖")
	    (:number "04" :name "一言寺")
	    (:number "05" :name "三宝院")
	    (:number "50" :name "点在")))
  (:name "乙訓" :number "50" 
   :branch ((:number "01" :name "長岡1")
	    (:number "02" :name "長岡2")
	    (:number "03" :name "長岡3")
	    (:number "04" :name "長岡4")
	    (:number "05" :name "物集女")
	    (:number "06" :name "勝山")
	    (:number "07" :name "西ノ岡")
	    (:number "08" :name "寺戸")
	    (:number "09" :name "森本")
	    (:number "10" :name "上植野")
	    (:number "11" :name "その他")
	    (:number "12" :name "神足")
	    (:number "13" :name "長岡5")
	    (:number "14" :name "奥海印寺")
	    (:number "15" :name "大山崎")))
  (:name "宇治" :number "51" 
   :branch ((:number "01" :name "三室戸")
	    (:number "02" :name "中宇治")
	    (:number "03" :name "上宇治")
	    (:number "05" :name "広野")
	    (:number "06" :name "大久保")
	    (:number "08" :name "伊勢田")
	    (:number "10" :name "西小倉")
	    (:number "11" :name "南小倉")
	    (:number "12" :name "北小倉")
	    (:number "13" :name "槙島")
	    (:number "14" :name "木幡")
	    (:number "16" :name "六地蔵")
	    (:number "17" :name "黄檗")
	    (:number "18" :name "北槙島")
	    (:number "19" :name "小倉")
	    (:number "20" :name "西槙島")
	    (:number "21" :name "神明北")
	    (:number "22" :name "神明南")
	    (:number "23" :name "岡屋")
	    (:number "50" :name "点在")))
  (:name "亀岡" :number "53" 
   :branch ((:number "01" :name "川東")
	    (:number "02" :name "東")
	    (:number "03" :name "川西")
	    (:number "04" :name "中")
	    (:number "05" :name "南")
	    (:number "06" :name "西")))
  (:name "船井" :number "54" 
   :branch ((:number "01" :name "八木")
	    (:number "02" :name "園部")
	    (:number "03" :name "丹波")
	    (:number "04" :name "日吉")
	    (:number "05" :name "和知")
	    (:number "06" :name "瑞穂")
	    (:number "07" :name "美山")
	    (:number "10" :name "点在")))
  (:name "綾部" :number "56" 
   :branch ((:number "12" :name "綾部")
	    (:number "13" :name "中筋")
	    (:number "14" :name "東")
	    (:number "15" :name "西")
	    (:number "16" :name "山家")
	    (:number "17" :name "上林")
	    (:number "18" :name "点在")))
  (:name "福知山" :number "57" 
   :branch ((:number "01" :name "福知山第一")
	    (:number "02" :name "福知山第二")
	    (:number "03" :name "大正")
	    (:number "04" :name "雀部")
	    (:number "05" :name "迂橋")
	    (:number "06" :name "六人部")
	    (:number "07" :name "庵我")
	    (:number "08" :name "川口")
	    (:number "09" :name "豊富")
	    (:number "10" :name "夜久野")
	    (:number "11" :name "大江")
	    (:number "12" :name "雲原")
	    (:number "13" :name "三和")
	    (:number "14" :name "点在")))
  (:name "舞鶴" :number "58" 
   :branch ((:number "02" :name "市場")
	    (:number "03" :name "祖母谷")
	    (:number "05" :name "南")
	    (:number "07" :name "行永")
	    (:number "08" :name "余部")
	    (:number "09" :name "城東")
	    (:number "10" :name "城南")
	    (:number "11" :name "城北")
	    (:number "12" :name "遠隔地")
	    (:number "13" :name "舞浜")
	    (:number "14" :name "志楽")
	    (:number "15" :name "朝来")))
  (:name "宮津" :number "59" 
   :branch ((:number "01" :name "城東")
	    (:number "03" :name "西")
	    (:number "05" :name "橋北")
	    (:number "08" :name "上宮津")
	    (:number "09" :name "栗田")
	    (:number "10" :name "岩滝")
	    (:number "11" :name "伊根")
	    (:number "12" :name "加悦谷")
	    (:number "15" :name "点在")))
  (:name "奥丹後" :number "60" 
   :branch ((:number "01" :name "網野")
	    (:number "02" :name "丹後")
	    (:number "03" :name "久美浜")
	    (:number "04" :name "大宮")
	    (:number "05" :name "峰山")
	    (:number "06" :name "弥栄")
	    (:number "07" :name "点在")))
  (:name "相楽" :number "61" 
   :branch ((:number "01" :name "山城")
	    (:number "02" :name "木津")
	    (:number "03" :name "精華")
	    (:number "04" :name "和束")
	    (:number "05" :name "笠置")
	    (:number "06" :name "加茂")
	    (:number "07" :name "南山城")))
  (:name "洛南" :number "62" 
   :branch ((:number "01" :name "長池青谷")
	    (:number "02" :name "富野")
	    (:number "04" :name "寺田東")
	    (:number "05" :name "寺田西")
	    (:number "08" :name "久津川")
	    (:number "11" :name "久御山東")
	    (:number "12" :name "久御山西")))
  (:name "綴喜八幡" :number "63" 
   :branch ((:number "01" :name "八幡東")
	    (:number "02" :name "八幡中")
	    (:number "03" :name "八幡西")
	    (:number "11" :name "田辺北")
	    (:number "12" :name "田辺中")
	    (:number "13" :name "田辺南")
	    (:number "15" :name "井出")
	    (:number "16" :name "宇治田原")
	    (:number "20" :name "三江")))
  (:name "表具" :number "90" 
   :branch ((:number "01" :name "表具")))
  (:name "電気" :number "95" 
   :branch ((:number "01" :name "左京区")
	    (:number "02" :name "北区")
	    (:number "03" :name "上京区")
	    (:number "04" :name "中京区")
	    (:number "05" :name "下京区")
	    (:number "06" :name "南区")
	    (:number "07" :name "東山区")
	    (:number "08" :name "山科区")
	    (:number "09" :name "右京区")
	    (:number "10" :name "西京区")
	    (:number "11" :name "伏見東")
	    (:number "12" :name "伏見")
	    (:number "20" :name "下部")
	    (:number "30" :name "西部")
	    (:number "40" :name "南部")
	    (:number "41" :name "宇治市")
	    (:number "50" :name "北部")
	    (:number "80" :name "管理")
	    (:number "90" :name "商業")
	    (:number "91" :name "その他")))))

(defun bunkai-hash ()
  (iterate:iter (iterate:with h = (make-hash-table :test #'equal))
		(iterate:for shibu :in shibu-all)
		(iterate:for shibu-number = (getf shibu :number))
		(iterate:for shibu-name   = (getf shibu :name))
		(iterate:iter (iterate:for data :in (getf shibu :branch))
			      (setf (gethash (format nil "~A~A" shibu-number (getf data :number)) h)
				    (getf data :name)))
		(iterate:finally (iterate::return h))))

(in-package :cl-user)
