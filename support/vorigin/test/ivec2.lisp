(in-package #:vorigin.test)

(define-test iv2/copy
  (let ((v (iv2:vec 1 2))
        (o (iv2:zero)))
    (is iv2:= (iv2:copy! o v) v)
    (is iv2:= o v)
    (is iv2:= (iv2:copy v) v)
    (isnt eq v (iv2:copy v))))

(define-test iv2/sign
  (let ((o (iv2:zero)))
    (is iv2:= (iv2:sign (iv2:zero)) (iv2:zero))
    (is iv2:= (iv2:sign (iv2:vec 10 10)) (iv2:vec 1 1))
    (is iv2:= (iv2:sign (iv2:vec -10 -10)) (iv2:vec -1 -1))
    (iv2:sign! o (iv2:zero))
    (is iv2:= o (iv2:zero))
    (iv2:sign! o (iv2:vec 10 10))
    (is iv2:= o (iv2:vec 1 1))
    (iv2:sign! o (iv2:vec -10 -10))
    (is iv2:= o (iv2:vec -1 -1))))

(define-test iv2/clamp
  (let ((v (iv2:vec -15 30))
        (r (iv2:vec -1 3))
        (o (iv2:zero)))
    (is iv2:= (iv2:clamp-range! o v -1 3) r)
    (is iv2:= o r)
    (is iv2:= (iv2:clamp-range v -1 3) r)
    (is iv2:= (iv2:clamp-range v (cl:- (cl:expt 2 31))
                                 (cl:1- (cl:expt 2 31)))
              v)))

(define-test iv2/zero
  (let ((v (iv2:vec -7 5)))
    (is iv2:= (iv2:zero! v) iv2:+zero+)
    (is iv2:= v iv2:+zero+)
    (is iv2:= (iv2:zero) iv2:+zero+)))

(define-test iv2/equality
  (let ((v1 (iv2:vec 8 -4))
        (v2 (iv2:vec 0 0)))
    (true (iv2:= v1 v1))
    (true (iv2:= (iv2:+ v1 v2) v1))
    (true (iv2:= v2 iv2:+zero+))))

(define-test iv2/add
  (let ((v1 (iv2:vec 4 -8))
        (v2 (iv2:vec 1 4))
        (r (iv2:vec 5 -4))
        (o (iv2:zero)))
    (is iv2:= (iv2:+! o v1 v2) r)
    (is iv2:= o r)
    (is iv2:= (iv2:+ v1 v2) r)
    (is iv2:= (iv2:+ v1 iv2:+zero+) v1)
    (is iv2:= (iv2:+ iv2:+zero+ v2) v2)))

(define-test iv2/subtract
  (let ((v1 (iv2:vec -1 8))
        (v2 (iv2:vec -2 7))
        (r (iv2:vec 1 1))
        (o (iv2:zero)))
    (is iv2:= (iv2:-! o v1 v2) r)
    (is iv2:= o r)
    (is iv2:= (iv2:- v1 v2) r)
    (is iv2:= (iv2:- v1 iv2:+zero+) v1)))

(define-test iv2/hadamard-product
  (let ((v1 (iv2:vec 3 10))
        (v2 (iv2:vec 3 -2))
        (r (iv2:vec 9 -20))
        (o (iv2:zero)))
    (is iv2:= (iv2:*! o v1 v2) r)
    (is iv2:= o r)
    (is iv2:= (iv2:* v1 v2) r)
    (is iv2:= (iv2:* v1 iv2:+zero+) iv2:+zero+)
    (is iv2:= (iv2:* iv2:+zero+ v2) iv2:+zero+)))

(define-test iv2/hadamard-quotient
  (let ((v1 (iv2:vec 50 36))
        (v2 (iv2:vec 2 3))
        (r (iv2:vec 25 12))
        (o (iv2:zero)))
    (is iv2:= (iv2:/! o v1 v2) r)
    (is iv2:= o r)
    (is iv2:= (iv2:/ v1 v2) r)
    (is iv2:= (iv2:/ v1 iv2:+zero+) iv2:+zero+)
    (is iv2:= (iv2:/ iv2:+zero+ v2) iv2:+zero+)))

(define-test iv2/scalar-product
  (let ((v (iv2:vec 3 4))
        (r (iv2:vec 12 16))
        (o (iv2:zero)))
    (is iv2:= (iv2:scale! o v 4) r)
    (is iv2:= o r)
    (is iv2:= (iv2:scale v 4) r)))

(define-test iv2/dot-product
  (is cl:= (iv2:dot (iv2:vec 4 5) (iv2:vec 6 7)) 59)
  (is cl:= (iv2:dot (iv2:vec 1 0) (iv2:vec 0 1)) 0)
  (is cl:= (iv2:dot (iv2:vec 1 0) (iv2:vec 1 0)) 1)
  (is cl:= (iv2:dot (iv2:vec 1 0) (iv2:vec -1 0)) -1))

(define-test iv2/length
  (let ((m (iv2:length iv2:+zero+))
        (n (iv2:length (iv2:vec 7 0)))
        (o (iv2:length (iv2:vec 1 1))))
    (true (cl:<= (abs (- m 0f0)) 1f-7))
    (true (cl:<= (abs (- n 7f0)) 1f-7))
    (true (cl:<= (abs (- o (cl:sqrt 2))) 1f-7))))

(define-test iv2/round
  (let ((v (iv2:vec -10 2))
        (r (iv2:vec -10 2))
        (o (iv2:zero)))
    (is iv2:= (iv2:round! o v) r)
    (is iv2:= o r)
    (is iv2:= (iv2:round v) r)))

(define-test iv2/abs
  (let ((v (iv2:vec -5 3))
        (r (iv2:vec 5 3))
        (o (iv2:zero)))
    (is iv2:= (iv2:abs! o v) r)
    (is iv2:= o r)
    (is iv2:= (iv2:abs v) r)))

(define-test iv2/negate
  (let ((v (iv2:vec 12 -5))
        (r (iv2:vec -12 5))
        (o (iv2:zero)))
    (is iv2:= (iv2:negate! o v) r)
    (is iv2:= o r)
    (is iv2:= (iv2:negate v) r)))

(define-test iv2/angle
  (let ((angle (iv2:angle (iv2:vec 0 1) (iv2:vec 1 0))))
    (true (<= (abs (- angle const:pi/2)) 1f-7)))
  (let ((angle (iv2:angle (iv2:vec 1 0) (iv2:vec 1 1))))
    (true (<= (abs (- angle const:pi/4)) 1f-7))))

(define-test iv2/zero-predicate
  (true (iv2:zero-p iv2:+zero+))
  (true (iv2:zero-p (iv2:vec 0 0)))
  (true (iv2:zero-p (iv2:zero))))

(define-test iv2/lerp
  (let ((v1 (iv2:vec 10 20))
        (v2 (iv2:vec 20 40))
        (r (iv2:vec 15 30))
        (o (iv2:zero)))
    (is iv2:= (iv2:lerp! o v1 v2 0.5f0) r)
    (is iv2:= o r)
    (is iv2:= (iv2:lerp v1 v2 0.5f0) r)
    (is iv2:= (iv2:lerp v1 v2 0f0) v1)
    (is iv2:= (iv2:lerp v1 v2 1f0) v2)))

(define-test iv2/compare
  (let ((v1 (iv2:vec 3 -4))
        (v2 (iv2:vec 6 -1))
        (v3 (iv2:vec 9 8))
        (v4 (iv2:vec 1 2))
        (v5 (iv2:vec 3 4)))
    (true (iv2:< v2 v3))
    (true (iv2:<= v4 v4))
    (true (iv2:<= v4 v5))
    (true (iv2:> v3 v1))
    (true (iv2:>= v4 v4))
    (true (iv2:>= v5 v4))))

(define-test iv2/min
  (let* ((v1 (iv2:vec 9 1))
         (v2 (iv2:vec 8 2))
         (r (iv2:vec (aref v2 0) (aref v1 1)))
         (o (iv2:zero)))
    (is iv2:= (iv2:min! o v1 v2) r)
    (is iv2:= o r)
    (is iv2:= (iv2:min v1 v2) r)))

(define-test iv2/max
  (let* ((v1 (iv2:vec 7 3))
         (v2 (iv2:vec 6 5))
         (r (iv2:vec (aref v1 0) (aref v2 1)))
         (o (iv2:zero)))
    (is iv2:= (iv2:max! o v1 v2) r)
    (is iv2:= o r)
    (is iv2:= (iv2:max v1 v2) r)))
