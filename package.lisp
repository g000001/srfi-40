;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-40
  (:use)
  (:export :stream-null
           :stream-cons
           :stream?
           :stream-null?
           :stream-pair?
           :stream-car
           :stream-cdr
           :stream-delay
           :stream
           :stream-unfoldn
           :stream-map
           :stream-for-each
           :stream-filter ))

(defpackage :srfi-40.internal
  (:use :srfi-40
        :cl
        :mbe
        :fiveam
        :srfi-5
        :srfi-23
        :srfi-9)
  (:shadowing-import-from :srfi-5
                          :let)
  (:shadowing-import-from :srfi-23
                          :error)
  (:shadowing-import-from :srfi-40
                          :stream)
  (:shadow :stream-error :loop))
