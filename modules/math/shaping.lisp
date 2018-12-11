(in-package :first-light.math)

(defun linear (x)
  x)

(defun sine-out (x)
  (cl:sin (cl:* pi 0.5 x)))

(defun sine-in (x)
  (1+ (cl:sin (cl:* (1- x) 0.5 pi))))

(defun sine-in-out (x)
  (cl:* 0.5 (- 1 (cl:cos (cl:* x pi)))))

(defun quadratic-out (x)
  (cl:- (cl:* x (cl:- x 2))))

(defun quadratic-in (x)
  (cl:* x x))

(defun quadratic-in-out (x)
  (if (cl:< x 0.5)
      (cl:* x x 2)
      (1- (cl:+ (cl:* x x -2) (cl:* x 4.0)))))

(defun  cubic-out (x)
  (let ((f (1- x)))
    (1+ (cl:expt f 3))))

(defun cubic-in (x)
  (cl:expt x 3))

(defun cubic-in-out (x)
  (if (cl:< x 0.5)
      (cl:* (cl:expt x 3) 4)
      (1+ (cl:* 0.5 (cl:expt (cl:- (cl:* x 2) 2) 3)))))

(defun quartic-out (x)
  (1+ (cl:* (cl:expt (1- x) 3) (cl:- 1 x))))

(defun quartic-in (x)
  (cl:expt x 4))

(defun quartic-in-out (x)
  (if (cl:< x 0.5)
      (cl:* (cl:expt x 4) 8)
      (1+ (cl:* -8 (cl:expt (1- x) 4)))))

(defun quintic-out (x)
  (1+ (cl:expt (1- x) 5)))

(defun quintic-in (x)
  (cl:expt x 5))

(defun quintic-in-out (x)
  (if (cl:< x 0.5)
      (cl:* (cl:expt x 5) 16)
      (1+ (cl:* 0.5 (cl:expt (cl:- (cl:* x 2) 2) 5)))))

(defun exponential-out (x)
  (if (cl:= x 1)
      x
      (cl:- 1 (cl:expt 2 (cl:* x -10)))))

(defun exponential-in (x)
  (if (zerop x)
      x
      (cl:expt 2 (cl:* 10 (1- x)))))

(defun exponential-in-out (x)
  (cond
    ((or (zerop x) (cl:= x 1))
     x)
    ((cl:< x 0.5)
     (cl:* 0.5 (cl:expt 2 (cl:- (cl:* x 20) 10))))
    (t
     (1+ (cl:* -0.5 (cl:expt 2 (cl:+ (cl:* x -20) 10)))))))

(defun circular-out (x)
  (cl:sqrt (cl:* (cl:- 2 x) x)))

(defun circular-in (x)
  (cl:- 1 (cl:sqrt (cl:- 1 (cl:* x x)))))

(defun circular-in-out (x)
  (if (cl:< x 0.5)
      (cl:* 0.5 (cl:- 1 (cl:sqrt (cl:- 1 (cl:* 4 x x)))))
      (cl:* 0.5 (1+ (cl:sqrt (cl:- (cl:* (cl:- (cl:* 2 x) 3) (1- (cl:* 2 x)))))))))

(defun back-out (x)
  (let ((v (cl:- 1 x)))
    (cl:- 1 (cl:- (cl:expt v 3) (cl:* v (cl:sin (cl:* v pi)))))))

(defun back-in (x)
  (cl:- (cl:expt x 2) (cl:* x (cl:sin (cl:* x pi)))))

(defun back-in-out (x)
  (if (cl:< x 0.5)
      (let ((v (cl:* x 2)))
        (cl:* 0.5 (cl:- (cl:expt v 3) (cl:* v (cl:sin (cl:* v pi))))))
      (let ((v (cl:- 2 (cl:* x 2))))
        (cl:+ (cl:* 0.5 (cl:- 1 (cl:- (cl:expt v 3) (cl:* v (cl:sin (cl:* v pi)))))) 0.5))))

(defun elastic-out (x)
  (1+ (cl:* (cl:sin (cl:* -13 pi 0.5 (1+ x))) (cl:expt 2 (cl:* -10 x)))))

(defun elastic-in (x)
  (cl:* (cl:sin (cl:* 13 pi 0.5 x)) (cl:expt 2 (cl:* 10 (1- x)))))

(defun elastic-in-out (x)
  (if (cl:< x 0.5)
      (let ((v (cl:* x 2)))
        (cl:* 0.5 (cl:sin (cl:* 13 pi 0.5 v)) (cl:expt 2 (cl:* 10 (1- v)))))
      (let ((v (1- (cl:* x 2))))
        (cl:* 0.5 (cl:+ (cl:* (cl:sin (cl:* -13 pi 0.5 (1+ v))) (cl:expt 2 (cl:* -10 v))) 2)))))

(defun bounce-out (x)
  (cond
    ((cl:< x 0.36363637)
     (cl:/ (cl:* x x 121) 16))
    ((cl:< x 0.72727275)
     (cl:+ (cl:- (cl:* 9.075 x x) (cl:* 9.9 x)) 3.4))
    ((cl:< x 0.9)
     (cl:+ (cl:- (cl:* 12.066482 x x) (cl:* 19.635458 x)) 8.898061))
    (t
     (cl:+ (cl:- (cl:* 10.8 x x) (cl:* 20.52 x)) 10.72))))

(defun bounce-in (x)
  (cl:- 1 (bounce-out (cl:- 1 x))))

(defun bounce-in-out (x)
  (let ((v (cl:* x 2)))
    (if (cl:< x 0.5)
        (cl:* 0.5 (bounce-in v))
        (cl:+ 0.5 (cl:* 0.5 (bounce-out (1- v)))))))

(defun hermite-curve (x)
  (cl:* x x (cl:- 3 (cl:* 2 x))))

(defun quintic-curve (x)
  (cl:* (cl:expt x 3) (cl:+ (cl:* x (cl:- (cl:* x 6) 15)) 10)))
