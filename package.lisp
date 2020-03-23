;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-40"
  (:use)
  (:export stream-null
           stream-cons
           stream?
           stream-null?
           stream-pair?
           stream-car
           stream-cdr
           stream-delay
           stream
           stream-unfoldn
           stream-map
           stream-for-each
           stream-filter ))


(defpackage "https://github.com/g000001/srfi-40#internals"
  (:use 
   "https://github.com/g000001/srfi-40"
   "https://github.com/g000001/srfi-5"
   "https://github.com/g000001/srfi-23"
   "https://github.com/g000001/srfi-9"
   cl
   mbe
   fiveam)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-5"
   let)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-23"
   error)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-40"
   stream)
  (:shadow stream-error loop))


;;; *EOF*
