(in-package :kensin)

(defvar reverse-dir #P"d:/特定健診結果データ/特定健診整理番号付加済み/")

(cl-irregsexp:match-bind (directory "/" hospital "_" oday "_" title "（" cday "_" ctime "）.csv")
    "d:/特定健診結果データ/特定健診整理番号付加済み/一般財団法人京都工場保健会_20130402_保健指導番号（20130819_121303）.csv"
  )

(in-package :cl-user)
