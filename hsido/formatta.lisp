(in-package :hsido)

(defmethod formatta1 ((h hsido))
  (with-hsido-slot h
    (list jnumber kigo bango name birth hname
	  created occurd rnumber firstd finald
	  (if hit "○" "×")
	  (if 172hit2012 "○" "×")
	  (if 172hit2011 "○" "×")
	  kmes2012 kmes2011 hmes2012 hmes2011)))

(defmethod formatta2 ((h hsido))
  (with-hsido-slot h
    (list zipname xmlname hname jnumber rnumber kigo bango name
	  occurd firstd finald
	  (if hit "○" "×")
	  (if 172hit2012 "○" "×")
	  (if 172hit2011 "○" "×"))))

(defun title1 ()
  (list "受診券番号" "記号" "番号" "名前" "生年月日" "医療機関"
	"作成日" "実施日" "利用券番号" "初回日" "評価日" "アップ済" "2012年172" "2011年172"
	"指導M2012" "指導M2011" "健診M2012" "健診M2011"))

(defun title2 ()
  (list "zip" "xml" "院所" "受診券番号" "利用券番号" "記号" "番号" "名前"
	"実施日" "初回" "最終" "アップ済" "172-2012" "172-2011"))

(defun titlefn (arg)
  (case arg
    (1 #'title1)
    (2 #'title2)))

(defun formattafn (arg)
  (case arg
    (1 #'formatta1)
    (2 #'formatta2)))

