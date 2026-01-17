(in-package #:vorigin.common)

(defun make-accessor-symbol (prefix &rest args)
  (u:format-symbol (symbol-package prefix) "~@:(~{~a~}~)" (cons prefix args)))

(defmacro = (x y &optional (rel 1e-7) (abs 1e-7))
  (u:once-only (x y)
    `(< (abs (- ,x ,y)) (max ,abs (* ,rel (max (abs ,x) (abs ,y)))))))

(defmacro cwset (count out subst &body body)
  `(psetf
    ,@(loop :for i :below count
            :append `((aref ,out ,i)
                      ,@(u:tree-leaves
                         body
                         (lambda (x) (and (symbolp x) (member x (u:ensure-list subst))))
                         (lambda (x) `(aref ,x ,i)))))))

(defmacro cwcmp (count subst &body body)
  `(and
    ,@(loop :for i :below count
            :append `(,@(u:tree-leaves
                         body
                         (lambda (x) (and (symbolp x) (member x (u:ensure-list subst))))
                         (lambda (x) `(aref ,x ,i)))))))

(defmacro cwcmp-or (count subst &body body)
  `(or
    ,@(loop :for i :below count
            :append `(,@(u:tree-leaves
                         body
                         (lambda (x) (and (symbolp x) (member x (u:ensure-list subst))))
                         (lambda (x) `(aref ,x ,i)))))))


;; General math utilities for signed modular mathematics

;; These are written in accordance to this url:
;; https://www.sbcl.org/manual/#Signed-modular-arithmetic
;; I've checked the assembly (at the time of writing this) and get the
;; recognized and optimized forms I expect.

(u:fn-> smod32+ (u:b32 u:b32) u:b32)
(declaim (inline smod32+))
(defun smod32+ (a b)
  (declare (type u:b32 a b))
  (let ((u (ldb (byte 32 0) (+ a b))))
    (logior u (- (mask-field (byte 1 31) u)))))

(u:fn-> smod32- (u:b32 u:b32) u:b32)
(declaim (inline smod32-))
(defun smod32- (a b)
  (declare (type u:b32 a b))
  (let ((u (ldb (byte 32 0) (- a b))))
    (logior u (- (mask-field (byte 1 31) u)))))

(u:fn-> smod32* (u:b32 u:b32) u:b32)
(declaim (inline smod32*))
(defun smod32* (a b)
  (declare (type u:b32 a b))
  (let ((u (ldb (byte 32 0) (* a b))))
    (logior u (- (mask-field (byte 1 31) u)))))

(u:fn-> smod32/ (u:b32 u:b32) u:b32)
(declaim (inline smod32/))
(defun smod32/ (a b)
  (declare (type u:b32 a b))
  (let ((u (ldb (byte 32 0) (truncate a b))))
    (logior u (- (mask-field (byte 1 31) u)))))

(u:fn-> smod32-mod (u:b32 u:b32) u:b32)
(declaim (inline smod32-mod))
(defun smod32-mod (a b)
  (declare (type u:b32 a b))
  (let ((u (ldb (byte 32 0) (mod a b))))
    (logior u (- (mask-field (byte 1 31) u)))))
