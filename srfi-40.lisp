;;;; srfi-40.lisp

(cl:in-package :srfi-40.internal)

;;; PROMISES A LA SRFI-45:

;;; A separate implementation is necessary to
;;; have promises that answer #t to stream?
;;; This requires lots of complicated type conversions.

(define-record-type s..promise (make-s>>promise kind content) s..promise?
  (kind    s>>promise-kind    set-s>>promise-kind!)
  (content s>>promise-content set-s>>promise-content!) )

(define-record-type box (make-box x) box?
  (x unbox set-box!) )

(define-syntax srfi-40>>lazy
  (syntax-rules ()
    ((srfi-40>>lazy exp)
     (make-box (make-s>>promise 'lazy (lambda () exp))) )))

(defun srfi-40>>eager (x)
  (make-stream (make-box (make-s>>promise 'eager x))) )

(define-syntax srfi-40>>delay
  (syntax-rules ()
    ((srfi-40>>delay exp) (srfi-40>>lazy (srfi-40>>eager exp))) ))

(defun srfi-40>>force (promise)
  (let ((content (unbox promise)))
    (case (s>>promise-kind content)
      ((eager) (s>>promise-content content))
      ((lazy)
       (let* ((promise* (stream-promise (funcall (s>>promise-content content))))
              (content  (unbox promise)) )
         (if (not (eql 'eager (s>>promise-kind content)))
             (progn
               (set-s>>promise-kind! content (s>>promise-kind (unbox promise*)))
               (set-s>>promise-content! content (s>>promise-content (unbox promise*)))
               (set-box! promise* content) ))
         (srfi-40>>force promise) )))))

;;; STREAM -- LIBRARY OF SYNTAX AND FUNCTIONS TO MANIPULATE STREAMS

;;; A stream is a new data type, disjoint from all other data types, that
;;; contains a promise that, when forced, is either nil (a single object
;;; distinguishable from all other objects) or consists of an object
;;; (the stream element) followed by a stream.  Each stream element is
;;; evaluated exactly once, when it is first retrieved (not when it is
;;; created); once evaluated its value is saved to be returned by
;;; subsequent retrievals without being evaluated again.

;; STREAM-TYPE -- type of streams
;; STREAM? object -- #t if object is a stream, #f otherwise
(define-record-type stream-type
  (make-stream promise)
  stream?
  (promise stream-promise) )

;;; UTILITY FUNCTIONS

;; STREAM-ERROR message -- print message then abort execution
;  replace this with a call to the native error handler
;  if stream-error returns, so will the stream library function that called it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun stream-error (&rest args)
    (apply #'error args) ))

;;; STREAM SYNTAX AND FUNCTIONS

;; STREAM-NULL -- the distinguished nil stream
(or (boundp 'stream-null)
    (defconstant stream-null (make-stream (srfi-40>>delay '()))))

;; STREAM-CONS object stream -- primitive constructor of streams
(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons obj strm)
     (make-stream
      (srfi-40>>delay
       (if (not (stream? strm))
           (stream-error "attempt to stream-cons onto non-stream")
           (cons obj strm)))))))

;; STREAM-NULL? object -- #t if object is the null stream, #f otherwise
(defun stream-null? (obj)
  (and (stream? obj) (null (srfi-40>>force (stream-promise obj)))))

;; STREAM-PAIR? object -- #t if object is a non-null stream, #f otherwise
(defun stream-pair? (obj)
  (and (stream? obj) (not (null (srfi-40>>force (stream-promise obj))))))

;; STREAM-CAR stream -- first element of stream
(defun stream-car (strm)
  (cond ((not (stream? strm)) (stream-error "attempt to take stream-car of non-stream"))
        ((stream-null? strm)  (stream-error "attempt to take stream-car of null stream"))
        (:else (car (srfi-40>>force (stream-promise strm))))))

;; STREAM-CDR stream -- remaining elements of stream after first
(defun stream-cdr (strm)
  (cond ((not (stream? strm)) (stream-error "attempt to take stream-cdr of non-stream"))
        ((stream-null? strm)  (stream-error "attempt to take stream-cdr of null stream"))
        (:else (cdr (srfi-40>>force (stream-promise strm))))))

;; STREAM-DELAY object -- the essential stream mechanism
(define-syntax stream-delay
  (syntax-rules ()
    ((stream-delay expr)
      (make-stream
        (srfi-40>>lazy expr)))))

;; STREAM object ... -- new stream whose elements are object ...
(defun stream (&rest objs)
  (let loop ((objs objs))
    (stream-delay
      (if (null objs)
          stream-null
          (stream-cons (car objs) (loop (cdr objs)))))))

;; STREAM-UNFOLDN generator seed n -- n+1 streams from (generator seed)
(defun stream-unfoldn (gen seed n)
  (labels ((unfold-result-stream (gen seed)
             (let loop ((seed seed))
                  (stream-delay
                   (multiple-value-call (lambda (next &rest results)
                                          (stream-cons results (loop next)) )
                                        (funcall gen seed)))))
           (result-stream->output-stream (result-stream i)
             (stream-delay
              (let ((result (nth i (stream-car result-stream))))
                (cond ((consp result)
                       (stream-cons (car result)
                                    (result-stream->output-stream
                                     (stream-cdr result-stream) i)))
                      ;; FIX: nil = '()
                      ((eq :false result)
                       (result-stream->output-stream (stream-cdr result-stream) i) )
                      ((null result) stream-null)
                      (:else (stream-error "can't happen")) ))))
           (result-stream->output-streams (result-stream n)
             (let loop ((i 0) (outputs '()))
                  (if (= i n)
                      (apply #'values (reverse outputs))
                      (loop (+ i 1)
                            (cons (result-stream->output-stream result-stream i)
                                  outputs ))))))
    (result-stream->output-streams (unfold-result-stream gen seed) n) ))

;; STREAM-MAP func stream ... -- stream produced by applying func element-wise
(defun stream-map (func &rest strms)
  (cond ((not (functionp func))
         (stream-error "non-functional argument to stream-map"))
        ((null strms)
         (stream-error "no stream arguments to stream-map"))
        ((not (every #'stream? strms)
              ) (stream-error "non-stream argument to stream-map"))
        (:else (let loop ((strms strms))
                (stream-delay
                  (if (some #'stream-null? strms)
                      stream-null
                      (stream-cons (apply func (mapcar #'stream-car strms))
                                   (loop (mapcar #'stream-cdr strms)))))))))

;; STREAM-FOR-EACH proc stream ... -- apply proc element-wise for side-effects
(defun stream-for-each (proc &rest strms)
  (cond ((not (functionp proc))
         (stream-error "non-functional argument to stream-for-each"))
        ((null strms)
         (stream-error "no stream arguments to stream-for-each"))
        ((not (every #'stream? strms))
         (stream-error "non-stream argument to stream-for-each"))
        (:else (let loop ((strms strms))
                    (if (not (some #'stream-null? strms))
                        (progn (apply proc (mapcar #'stream-car strms))
                               (loop (mapcar #'stream-cdr strms))))))))

;; STREAM-FILTER pred? stream -- new stream including only items passing pred?
(defun stream-filter (pred? strm)
  (cond ((not (functionp pred?))
         (stream-error "non-functional argument to stream-filter") )
        ((not (stream? strm))
         (stream-error "attempt to apply stream-filter to non-stream") )
        (:else (stream-unfoldn
                (lambda (s)
		  (cond
                    ((stream-null? s)
                     (values stream-null '()) )
                    ((funcall pred? (stream-car s))
                     (values (stream-cdr s) (list (stream-car s))) )
                    (:else
                     (values (stream-cdr s) :false) )))
                strm
                1 ))))

;;; eof
