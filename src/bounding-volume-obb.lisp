(in-package #:virality.engine)

(defclass oriented-bounding-box ()
  ((%center :reader center
            :initarg :center
            :initform (v3:vec))
   (%axes :reader axes
          :initarg :axes
          :initform (m3:mat))
   (%half-widths :reader half-widths
                 :initarg :half-widths
                 :initform (v3:vec))))

(defun make-oriented-bounding-box (center axes half-widths)
  (make-instance 'oriented-bounding-box
                 :center center
                 :axes axes
                 :half-widths half-widths))

(defun get-closest-point/obb-point (obb point)
  (with-slots (%center %axes %half-widths) obb
    (let ((d (v3:- point %center))
          (q (v3:copy %center)))
      (dotimes (i 3)
        (let ((dist (v3:dot d (m3:get-column %axes i)))
              (e (aref %half-widths i)))
          (when (> dist e)
            (setf dist e))
          (when (< dist (- e))
            (setf dist (- e)))
          (v3:+! q q (v3:scale (m3:get-column %axes i) dist))))
      q)))

(defun make-obb-obb-rotation (obb1 obb2)
  (let ((axes1 (axes obb1))
        (axes2 (axes obb2)))
    (m3:with-components ((a (m3:mat 1))
                         (b (m3:mat 1)))
      (psetf a00 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 0))
             a10 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 0))
             a20 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 0))
             a01 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 1))
             a11 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 1))
             a21 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 1))
             a02 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 2))
             a12 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 2))
             a22 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 2))
             b00 (+ (abs a00) 1e-7)
             b10 (+ (abs a10) 1e-7)
             b20 (+ (abs a20) 1e-7)
             b01 (+ (abs a01) 1e-7)
             b11 (+ (abs a11) 1e-7)
             b21 (+ (abs a21) 1e-7)
             b02 (+ (abs a02) 1e-7)
             b12 (+ (abs a12) 1e-7)
             b22 (+ (abs a22) 1e-7))
      (values a b))))

(defun make-obb-obb-translation (obb1 obb2)
  (let ((axes1 (axes obb1))
        (translation (v3:- (center obb2) (center obb1))))
    (v3:vec (v3:dot translation (m3:get-column axes1 0))
            (v3:dot translation (m3:get-column axes1 1))
            (v3:dot translation (m3:get-column axes1 2)))))
