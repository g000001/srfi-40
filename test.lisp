(cl:in-package "https://github.com/g000001/srfi-40#internals")


(def-suite* srfi-40)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun stream-to-list (srm)
    (do ((srm srm (stream-cdr srm))
         (ans '() (let ((scar (stream-car srm)))
                    (cons (if (stream? scar)
                              (stream-to-list scar)
                              scar)
                          ans))) )
        ((stream-null? srm)
         (nreverse ans) )))
  (defun stream-equal (x y)
    (apply #'equal (mapcar #'stream-to-list (list x y)))))


(test stream-null
  (is (stream-equal stream-null (stream))))


(test stream-cons
  (is (stream-equal (stream-cons 'a stream-null)
                    (stream 'a)))
  (is (stream-equal (stream-cons 'a (stream 'b 'c 'd))
                    (stream 'a 'b 'c 'd)))
  (is (stream-equal (stream-cons "a" (stream 'b 'c))
                    (stream "a" 'b 'c)))
  (signals (cl:error)
    (stream-equal (stream-cons 'a 3)))
  (is (stream-equal (stream-cons (stream 'a 'b) (stream 'c))
                    (stream (stream 'a 'b) 'c))))


(test stream?
  (is-true (stream? stream-null))
  (is-true (stream? (stream-cons 'a stream-null)))
  (is-false (stream? 3)))


(test stream-null?
  (is-true (stream-null? stream-null))
  (is-false (stream-null? (stream-cons 'a stream-null)))
  (is-false (stream-null? 3)))


(test stream-pair?
  (is-false (stream-pair? stream-null))
  (is-true (stream-pair? (stream-cons 'a stream-null)))
  (is-false (stream-pair? 3)))


(test stream-car
  (is (eq 'a (stream-car (stream 'a 'b 'c))))
  (signals (cl:error) (stream-car stream-null))
  (signals (cl:error) (stream-car 3)))


(test stream-cdr
  (is (equal (stream-to-list (stream-cdr (stream 'a 'b 'c)))
             (stream-to-list (stream 'b 'c))))
  (signals (cl:error) (stream-cdr stream-null) )
  (signals (cl:error) (stream-cdr 3)))


(defvar from0
  (let loop ((x 0))
       (stream-delay
        (stream-cons x (loop (+ x 1))))))


(test stream-delay
  (is (= #0=10000
         (let ((p nil))
           (setq p from0)
           (dotimes (i #0#)
             (setq p (stream-cdr p)))
           (stream-car p)))))


(test stream
  (is (stream-equal (stream 'a (+ 3 4) 'c)
                    (stream 'a 7 'c))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun take5 (s)
    (stream-unfoldn
     (lambda (x)
       (let ((n (car x)) (s (cdr x)))
         (if (zerop n)
             (values 'dummy '())
             (values
              (cons (- n 1) (stream-cdr s))
              (list (stream-car s))))))
     (cons 5 s)
     1)))


(test stream-unfoldn
  (is (stream-equal (take5 from0)
                    (stream 0 1 2 3 4) )))


(test stream-map
  (is (stream-equal (stream-map (lambda (x) (+ x x)) (take5 from0))
                    (stream 0 2 4 6 8 )))
  (is (stream-equal (stream-map #'+ (stream 1 2 3) (stream 4 5 6))
                    (stream 5 7 9)))
  (is (stream-equal (stream-map (lambda (x) (expt x x))
                                (stream 1 2 3 4 5))
                    (stream 1 4 27 256 3125))))


(test stream-for-each
  (is (string= (with-output-to-string (*standard-output*)
                 (stream-for-each #'princ (take5 from0)))
               "01234")))


(test stream-filter
  (is (stream-equal (stream-filter #'oddp stream-null)
                    stream-null))
  (is (stream-equal (take5 (stream-filter #'oddp from0))
                    (stream 1 3 5 7 9))))


;;; *EOF*
