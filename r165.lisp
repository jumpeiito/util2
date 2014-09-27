(defpackage #:r165
  (:use :cl :util :kensin :iterate))

(in-package #:r165)
(defparameter file ksetting::*fkac165*)

(defstruct data
  kigo bango birth gender id rnumber hcode hname level
  firstd finald interrupt)

(defun line->data (line)
  (line-binding
   (line data)
   (_ kigo bango _ birth gender id _ _ _ rnumber _ _ hcode level _ _ firstd)
   (with-slots (finald interrupt hname) obj
     (setq finald	(nth 67 line)
	   interrupt	(nth 98 line)
	   hname	(kensin::code->hospital hcode)
	   birth	(strdt birth)
	   firstd	(strdt firstd)
	   finald	(strdt finald)
	   interrupt    (strdt interrupt)))))

(defparameter testline
  (ppcre:split ","
	       "00263129,建１３中１２,０２０７７,3,19650625,1,0870404601,803996243,2012,1,12211200450,1,20120622,2610307411,1,3,じっくりコース,20120622,1,30,3,24,92.0,72.0,127,80,240,80,160,4,2,60,0,0,0,0,0,0,0,2,10,0,240,20,260,2,5,20120702,5,10,3,2610307411,京都民医連太子道診療所,20121012,1,30,120,3,91.5,71.9,123,89,1,1,2,2610307411,京都民医連太子道診療所,20130308,1,3,0,94.0,76.1,115,89,1,1,2,2610307411,京都民医連太子道診療所,4,2,60,0,0,0,0,0,0,0,2,10,0,240,20,260,0,20130308,,00263129,12112004501,00263129,12211200450"))

(in-package :kensin)

(defun r165-iterate (func)
  (util::csv-read-iter
   r165::file
   (lambda (line)
     (optima:match line
       ((LIST* _ "FKAC165"  _) :ignore)	; 1行目
       ((LIST* "保険者番号" _) :ignore)	; 2行目
       ((LIST _)               :ignore)	; 最終行
       (_
	(funcall func (r165::line->data line)))))
   :code :UTF-8))

(defun r165-hash ()
  (let ((hash (make-hash-table :test #'equal)))
    (r165-iterate
     (lambda (r165)
       (setf (gethash (r165::data-rnumber r165) hash)
	     r165)))
    hash))
