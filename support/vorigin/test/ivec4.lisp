(in-package #:vorigin.test)

(define-test iv4/copy
  (let ((v (iv4:vec 1 2 3 4))
        (o (iv4:zero)))
    (is iv4:= (iv4:copy! o v) v)
    (is iv4:= o v)
    (is iv4:= (iv4:copy v) v)
    (isnt eq v (iv4:copy v))))

(define-test iv4/sign
  (let ((o (iv4:zero)))
    (is iv4:= (iv4:sign (iv4:zero)) (iv4:zero))
    (is iv4:= (iv4:sign (iv4:vec 10 10 10 10)) (iv4:vec 1 1 1 1))
    (is iv4:= (iv4:sign (iv4:vec -10 -10 -10 -10)) (iv4:vec -1 -1 -1 -1))
    (iv4:sign! o (iv4:zero))
    (is iv4:= o (iv4:zero))
    (iv4:sign! o (iv4:vec 10 10 10 10))
    (is iv4:= o (iv4:vec 1 1 1 1))
    (iv4:sign! o (iv4:vec -10 -10 -10 -10))
    (is iv4:= o (iv4:vec -1 -1 -1 -1))))

(define-test iv4/clamp
  (let ((v (iv4:vec -15 30 -60 75))
        (r (iv4:vec -1 3 -1 3))
        (o (iv4:zero)))
    (is iv4:= (iv4:clamp-range! o v -1 3) r)
    (is iv4:= o r)
    (is iv4:= (iv4:clamp-range v -1 3) r)
    (is iv4:= (iv4:clamp-range v
                               (cl:- (cl:expt 2 31))
                               (cl:1- (cl:expt 2 31)))
        v)))

(define-test iv4/zero
  (let ((v (iv4:vec -7 5 0 10)))
    (is iv4:= (iv4:zero! v) iv4:+zero+)
    (is iv4:= v iv4:+zero+)
    (is iv4:= (iv4:zero) iv4:+zero+)))

(define-test iv4/equality
  (let ((v1 (iv4:vec 8 -4 -3 2))
        (v2 (iv4:vec 0 0 0 0)))
    (true (iv4:= v1 v1))
    (true (iv4:= (iv4:+ v1 v2) v1))
    (true (iv4:= v2 iv4:+zero+))))

(define-test iv4/add
  (let ((v1 (iv4:vec 4 -8 -6 0))
        (v2 (iv4:vec 1 4 7 9))
        (r (iv4:vec 5 -4 1 9))
        (o (iv4:zero)))
    (is iv4:= (iv4:+! o v1 v2) r)
    (is iv4:= o r)
    (is iv4:= (iv4:+ v1 v2) r)
    (is iv4:= (iv4:+ v1 iv4:+zero+) v1)
    (is iv4:= (iv4:+ iv4:+zero+ v2) v2)))

(define-test iv4/subtract
  (let ((v1 (iv4:vec -1 8 -8 2))
        (v2 (iv4:vec -2 7 -7 3))
        (r (iv4:vec 1 1 -1 -1))
        (o (iv4:zero)))
    (is iv4:= (iv4:-! o v1 v2) r)
    (is iv4:= o r)
    (is iv4:= (iv4:- v1 v2) r)
    (is iv4:= (iv4:- v1 iv4:+zero+) v1)))

(define-test iv4/hadamard-product
  (let ((v1 (iv4:vec 3 10 7 -3))
        (v2 (iv4:vec 3 -2 4 6))
        (r (iv4:vec 9 -20 28 -18))
        (o (iv4:zero)))
    (is iv4:= (iv4:*! o v1 v2) r)
    (is iv4:= o r)
    (is iv4:= (iv4:* v1 v2) r)
    (is iv4:= (iv4:* v1 iv4:+zero+) iv4:+zero+)
    (is iv4:= (iv4:* iv4:+zero+ v2) iv4:+zero+)))

(define-test iv4/hadamard-quotient
  (let ((v1 (iv4:vec 50 36 -24 -10))
        (v2 (iv4:vec 2 3 6 -5))
        (r (iv4:vec 25 12 -4 2))
        (o (iv4:zero)))
    (is iv4:= (iv4:/! o v1 v2) r)
    (is iv4:= o r)
    (is iv4:= (iv4:/ v1 v2) r)
    (is iv4:= (iv4:/ v1 iv4:+zero+) iv4:+zero+)
    (is iv4:= (iv4:/ iv4:+zero+ v2) iv4:+zero+)))

(define-test iv4/scalar-product
  (let ((v (iv4:vec 3 4 5 6))
        (r (iv4:vec 12 16 20 24))
        (o (iv4:zero)))
    (is iv4:= (iv4:scale! o v 4) r)
    (is iv4:= o r)
    (is iv4:= (iv4:scale v 4) r)))

(define-test iv4/dot-product
  (is = (iv4:dot (iv4:vec 4 5 8 9) (iv4:vec 6 7 10 11)) 238)
  (is = (iv4:dot (iv4:vec 1 0 0 0) (iv4:vec 0 1 0 0)) 0)
  (is = (iv4:dot (iv4:vec 1 0 0 0) (iv4:vec 0 0 1 0)) 0)
  (is = (iv4:dot (iv4:vec 0 1 0 0) (iv4:vec 0 0 1 0)) 0)
  (is = (iv4:dot (iv4:vec 1 0 0 0) (iv4:vec 1 0 0 0)) 1)
  (is = (iv4:dot (iv4:vec 1 0 0 0) (iv4:vec -1 0 0 0)) -1))

(define-test iv4/length
  (let ((m (iv4:length iv4:+zero+))
        (n (iv4:length (iv4:vec 7 0 0 0)))
        (o (iv4:length (iv4:vec 1 1 1 1))))
    (true (<= (abs (- m 0d0)) 1f-7))
    (true (<= (abs (- n 7f0)) 1f-7))
    (true (<= (abs (- o (sqrt 4))) 1f-7))))

(define-test iv4/round
  (let ((v (iv4:vec -10 2 5 -14))
        (r (iv4:vec -10 2 5 -14))
        (o (iv4:zero)))
    (is iv4:= (iv4:round! o v) r)
    (is iv4:= o r)
    (is iv4:= (iv4:round v) r)))

(define-test iv4/abs
  (let ((v (iv4:vec -5 3 4 -8))
        (r (iv4:vec 5 3 4 8))
        (o (iv4:zero)))
    (is iv4:= (iv4:abs! o v) r)
    (is iv4:= o r)
    (is iv4:= (iv4:abs v) r)))

(define-test iv4/negate
  (let ((v (iv4:vec 12 -5 34 -16))
        (r (iv4:vec -12 5 -34 16))
        (o (iv4:zero)))
    (is iv4:= (iv4:negate! o v) r)
    (is iv4:= o r)
    (is iv4:= (iv4:negate v) r)))

;; TODO: Implement iv4/angle test

(define-test iv4/zero-predicate
  (true (iv4:zero-p iv4:+zero+))
  (true (iv4:zero-p (iv4:vec 0 0 0 0)))
  (true (iv4:zero-p (iv4:zero))))

(define-test iv4/lerp
  (let ((v1 (iv4:vec 10 20 30 40))
        (v2 (iv4:vec 20 40 60 80))
        (r (iv4:vec 15 30 45 60))
        (o (iv4:zero)))
    (is iv4:= (iv4:lerp! o v1 v2 0.5f0) r)
    (is iv4:= o r)
    (is iv4:= (iv4:lerp v1 v2 0.5f0) r)
    (is iv4:= (iv4:lerp v1 v2 0f0) v1)
    (is iv4:= (iv4:lerp v1 v2 1f0) v2)))

(define-test iv4/compare
  (let ((v1 (iv4:vec 3 -4 -10 7))
        (v2 (iv4:vec 6 -1 8 -3))
        (v3 (iv4:vec 9 9 9 9))
        (v4 (iv4:vec 1 2 5 -1))
        (v5 (iv4:vec 3 4 6 0)))
    (true (iv4:< v2 v3))
    (true (iv4:<= v4 v4))
    (true (iv4:<= v4 v5))
    (true (iv4:> v3 v1))
    (true (iv4:>= v4 v4))
    (true (iv4:>= v5 v4))))

(define-test iv4/min
  (let* ((v1 (iv4:vec 9 1 -10 32))
         (v2 (iv4:vec 8 2 -1 3))
         (r (iv4:vec (aref v2 0) (aref v1 1) (aref v1 2) (aref v2 3)))
         (o (iv4:zero)))
    (is iv4:= (iv4:min! o v1 v2) r)
    (is iv4:= o r)
    (is iv4:= (iv4:min v1 v2) r)))

(define-test iv4/max
  (let* ((v1 (iv4:vec 7 3 -1 4))
         (v2 (iv4:vec 6 5 0 5))
         (r (iv4:vec (aref v1 0) (aref v2 1) (aref v2 2) (aref v2 3)))
         (o (iv4:zero)))
    (is iv4:= (iv4:max! o v1 v2) r)
    (is iv4:= o r)
    (is iv4:= (iv4:max v1 v2) r)))
