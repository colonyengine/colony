(in-package #:virality.engine)

(defun get-gpu-parameter (name)
  (or (handler-case (gl:get* name)
        (error (e)
          (declare (ignore e))
          :unavailable))
      :unavailable))

(defun get-gpu-vendor ()
  (let ((vendor (get-gpu-parameter :vendor)))
    (cond
      ((search "Intel" vendor) :intel)
      ((search "NVIDIA" vendor) :nvidia)
      ((search "AMD" vendor) :amd)
      ((search "ATI" vendor) :amd)
      (t :unknown))))

(defun check-texture-size (width height)
  (let ((max (get-gpu-parameter :max-texture-size)))
    (if (< max (max width height))
        (error "Hardware does not support the texture size: ~dx~d.~%
                Maximum supported size: ~dx~d." width height max max)
        t)))
