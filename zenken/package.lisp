(defpackage :kensin/zenken
  (:nicknames :zenken :kz)
  (:use :cl :util :iterate :cl-win32ole :excel)
  (:import-from :cl-fad #:file-exists-p))
