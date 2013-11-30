(defpackage #:r172
  (:use :cl :util :kensin :iterate :excel
	:cl-win32ole))

(in-package :r172)

(defclass SHEET ()
  ((book	:initarg :book)
   (sheet	:initarg :sheet :accessor sheet-of)
   (name	:initarg :name)
   (borders	:initarg :borders)
   (width	:initarg :width)
   (jpfont	:initarg :jpfont)
   (enfont	:initarg :enfont)
   (c-align	:initarg :c-align)
   (title	:initarg :title)
   (endcol	:initarg :endcol)
   (end		:initarg :end :reader end-of)
   (shibu-step	:initarg :shibu-step)
   (shibu-row	:initarg :shibu-row)
   (shibu-col	:initarg :shibu-col)))

(defclass TYPE1-SHEET (SHEET)
  ((data1	:initarg :data1)
   (data2	:initarg :data2)
   (step	:initarg :step :initform 5)
   (category	:initarg :category
		:reader category)
   (mainarray	:initarg :mainarray
		:reader array-of)))

(defclass SPEC-SHEET (SHEET)
  ((data	:initarg :data)
   (merge-cells :initarg :merge-cells)
   (array	:initarg :array)
   (dock	:initarg :dock)
   (sc		:initarg :sc)))

(defclass HSIDO-SHEET (SHEET)
  ((data	:initarg :data)
   (merge-cells :initarg :merge-cells)
   (hlv		:initarg :array)
   (sexmain	:initarg :sexmain)
   (hkmain	:initarg :hkmain)))

(defclass 167-SHEET (SHEET)
  ((data	:initarg :data)
   (merge-cells :initarg :merge-cells)
   (step1	:initarg :step1)
   (step2	:initarg :step2)))
