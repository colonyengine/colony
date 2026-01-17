(in-package #:vorigin.test)

(define-test iv3/copy
  (let ((v (iv3:vec 1 2 3))
        (o (iv3:zero)))
    (is iv3:= (iv3:copy! o v) v)
    (is iv3:= o v)
    (is iv3:= (iv3:copy v) v)
    (isnt eq v (iv3:copy v))))

(define-test iv3/sign
  (let ((o (iv3:zero)))
    (is iv3:= (iv3:sign (iv3:zero)) (iv3:zero))
    (is iv3:= (iv3:sign (iv3:vec 10 10 10)) (iv3:vec 1 1 1))
    (is iv3:= (iv3:sign (iv3:vec -10 -10 -10)) (iv3:vec -1 -1 -1))
    (iv3:sign! o (iv3:zero))
    (is iv3:= o (iv3:zero))
    (iv3:sign! o (iv3:vec 10 10 10))
    (is iv3:= o (iv3:vec 1 1 1))
    (iv3:sign! o (iv3:vec -10 -10 -10))
    (is iv3:= o (iv3:vec -1 -1 -1))))

(define-test iv3/clamp
  (let ((v (iv3:vec -15 30 -60))
        (r (iv3:vec -1 3 -1))
        (o (iv3:zero)))
    (is iv3:= (iv3:clamp-range! o v -1 3) r)
    (is iv3:= o r)
    (is iv3:= (iv3:clamp-range v -1 3) r)
    (is iv3:= (iv3:clamp-range v
                               (cl:- (cl:expt 2 31))
                               (cl:1- (cl:expt 2 31)))
        v)))

(define-test iv3/zero
  (let ((v (iv3:vec -7 5 0)))
    (is iv3:= (iv3:zero! v) iv3:+zero+)
    (is iv3:= v iv3:+zero+)
    (is iv3:= (iv3:zero) iv3:+zero+)))

(define-test iv3/equality
  (let ((v1 (iv3:vec 8 -4 -3))
        (v2 (iv3:vec 0 0 0)))
    (true (iv3:= v1 v1))
    (true (iv3:= (iv3:+ v1 v2) v1))
    (true (iv3:= v2 iv3:+zero+))))

(define-test iv3/add
  (let ((v1 (iv3:vec 4 -8 -6))
        (v2 (iv3:vec 1 4 7))
        (r (iv3:vec 5 -4 1))
        (o (iv3:zero)))
    (is iv3:= (iv3:+! o v1 v2) r)
    (is iv3:= o r)
    (is iv3:= (iv3:+ v1 v2) r)
    (is iv3:= (iv3:+ v1 iv3:+zero+) v1)
    (is iv3:= (iv3:+ iv3:+zero+ v2) v2)))

(define-test iv3/subtract
  (let ((v1 (iv3:vec -1 8 -8))
        (v2 (iv3:vec -2 7 -7))
        (r (iv3:vec 1 1 -1))
        (o (iv3:zero)))
    (is iv3:= (iv3:-! o v1 v2) r)
    (is iv3:= o r)
    (is iv3:= (iv3:- v1 v2) r)
    (is iv3:= (iv3:- v1 iv3:+zero+) v1)))

(define-test iv3/hadamard-product
  (let ((v1 (iv3:vec 3 10 7))
        (v2 (iv3:vec 3 -2 4))
        (r (iv3:vec 9 -20 28))
        (o (iv3:zero)))
    (is iv3:= (iv3:*! o v1 v2) r)
    (is iv3:= o r)
    (is iv3:= (iv3:* v1 v2) r)
    (is iv3:= (iv3:* v1 iv3:+zero+) iv3:+zero+)
    (is iv3:= (iv3:* iv3:+zero+ v2) iv3:+zero+)))

(define-test iv3/hadamard-quotient
  (let ((v1 (iv3:vec 50 36 -24))
        (v2 (iv3:vec 2 3 6))
        (r (iv3:vec 25 12 -4))
        (o (iv3:zero)))
    (is iv3:= (iv3:/! o v1 v2) r)
    (is iv3:= o r)
    (is iv3:= (iv3:/ v1 v2) r)
    (is iv3:= (iv3:/ v1 iv3:+zero+) iv3:+zero+)
    (is iv3:= (iv3:/ iv3:+zero+ v2) iv3:+zero+)))

(define-test iv3/scalar-product
  (let ((v (iv3:vec 3 4 5))
        (r (iv3:vec 12 16 20))
        (o (iv3:zero)))
    (is iv3:= (iv3:scale! o v 4) r)
    (is iv3:= o r)
    (is iv3:= (iv3:scale v 4) r)))

(define-test iv3/dot-product
  (is = (iv3:dot (iv3:vec 4 5 8) (iv3:vec 6 7 9)) 131)
  (is = (iv3:dot (iv3:vec 1 0 0) (iv3:vec 0 1 0)) 0)
  (is = (iv3:dot (iv3:vec 1 0 0) (iv3:vec 0 0 1)) 0)
  (is = (iv3:dot (iv3:vec 0 1 0) (iv3:vec 0 0 1)) 0)
  (is = (iv3:dot (iv3:vec 1 0 0) (iv3:vec 1 0 0)) 1)
  (is = (iv3:dot (iv3:vec 1 0 0) (iv3:vec -1 0 0)) -1))

(define-test iv3/cross-product
  (let ((v1 (iv3:vec 1 0 0))
        (v2 (iv3:vec 0 1 0))
        (o (iv3:zero)))
    (is iv3:= (iv3:cross! o v1 v2) (iv3:vec 0 0 1))
    (is iv3:= o (iv3:vec 0 0 1))
    (is iv3:= (iv3:cross (iv3:vec 1 0 0)
                         (iv3:vec 0 1 0))
        (iv3:vec 0 0 1))
    (is iv3:= (iv3:cross (iv3:vec 1 0 0)
                         (iv3:vec 0 0 1))
        (iv3:vec 0 -1 0))
    (is iv3:= (iv3:cross (iv3:vec 0 1 0)
                         (iv3:vec 1 0 0))
        (iv3:vec 0 0 -1))
    (is iv3:= (iv3:cross (iv3:vec 0 1 0)
                         (iv3:vec 0 0 1))
        (iv3:vec 1 0 0))
    (is iv3:= (iv3:cross (iv3:vec 0 0 1)
                         (iv3:vec 1 0 0))
        (iv3:vec 0 1 0))
    (is iv3:= (iv3:cross (iv3:vec 0 0 1)
                         (iv3:vec 0 1 0))
        (iv3:vec -1 0 0))))

(define-test iv3/length
  (let ((m (iv3:length iv3:+zero+))
        (n (iv3:length (iv3:vec 7 0 0)))
        (o (iv3:length (iv3:vec 1 1 1))))
    (true (<= (abs (- m 0f0)) 1f-7))
    (true (<= (abs (- n 7f0)) 1f-7))
    (true (<= (abs (- o (sqrt 3))) 1f-7))))

(define-test iv3/round
  (let ((v (iv3:vec -10 2 5))
        (r (iv3:vec -10 2 5))
        (o (iv3:zero)))
    (is iv3:= (iv3:round! o v) r)
    (is iv3:= o r)
    (is iv3:= (iv3:round v) r)))

(define-test iv3/abs
  (let ((v (iv3:vec -5 3 4))
        (r (iv3:vec 5 3 4))
        (o (iv3:zero)))
    (is iv3:= (iv3:abs! o v) r)
    (is iv3:= o r)
    (is iv3:= (iv3:abs v) r)))

(define-test iv3/negate
  (let ((v (iv3:vec 12 -5 34))
        (r (iv3:vec -12 5 -34))
        (o (iv3:zero)))
    (is iv3:= (iv3:negate! o v) r)
    (is iv3:= o r)
    (is iv3:= (iv3:negate v) r)))

(define-test iv3/angle
  (let ((angle (iv3:angle (iv3:vec 0 1 0) (iv3:vec 1 0 1))))
    (true (<= (abs (- angle const:pi/2)) 1f-5)))
  (let ((angle (iv3:angle (iv3:vec 1 1 0) (iv3:vec 1 0 1))))
    (true (<= (abs (- angle const:pi/3)) 1f-5)))
  (let ((angle (iv3:angle (iv3:vec 1 0 0) (iv3:vec 1 1 0))))
    (true (<= (abs (- angle const:pi/4)) 1f-5))))

(define-test iv3/zero-predicate
  (true (iv3:zero-p iv3:+zero+))
  (true (iv3:zero-p (iv3:vec 0 0 0)))
  (true (iv3:zero-p (iv3:zero))))

(define-test iv3/lerp
  (let ((v1 (iv3:vec 10 20 30))
        (v2 (iv3:vec 20 40 60))
        (r (iv3:vec 15 30 45))
        (o (iv3:zero)))
    (is iv3:= (iv3:lerp! o v1 v2 0.5f0) r)
    (is iv3:= o r)
    (is iv3:= (iv3:lerp v1 v2 0.5f0) r)
    (is iv3:= (iv3:lerp v1 v2 0f0) v1)
    (is iv3:= (iv3:lerp v1 v2 1f0) v2)))

(define-test iv3/compare
  (let ((v1 (iv3:vec 3 -4 -10))
        (v2 (iv3:vec 6 -1 8))
        (v3 (iv3:vec 9 9 9))
        (v4 (iv3:vec 1 2 5))
        (v5 (iv3:vec 3 4 6)))
    (true (iv3:< v2 v3))
    (true (iv3:<= v4 v4))
    (true (iv3:<= v4 v5))
    (true (iv3:> v3 v1))
    (true (iv3:>= v4 v4))
    (true (iv3:>= v5 v4))))

(define-test iv3/min
  (let* ((v1 (iv3:vec 9 1 -10))
         (v2 (iv3:vec 8 2 -1))
         (r (iv3:vec (aref v2 0) (aref v1 1) (aref v1 2)))
         (o (iv3:zero)))
    (is iv3:= (iv3:min! o v1 v2) r)
    (is iv3:= o r)
    (is iv3:= (iv3:min v1 v2) r)))

(define-test iv3/max
  (let* ((v1 (iv3:vec 7 3 -1))
         (v2 (iv3:vec 6 5 0))
         (r (iv3:vec (aref v1 0) (aref v2 1) (aref v2 2)))
         (o (iv3:zero)))
    (is iv3:= (iv3:max! o v1 v2) r)
    (is iv3:= o r)
    (is iv3:= (iv3:max v1 v2) r)))
