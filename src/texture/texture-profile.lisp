(in-package #:colony.texture)

;; Implementation of TEXTURE-PROFILE

;; TODO: Candidate for public API
(defun make-texture-profile (&rest init-args)
  (apply #'make-instance 'texture-profile init-args))

(defun parse-texture-profile (name body-form)
  (u:with-gensyms (texprof)
    `(let* ((,texprof (make-texture-profile :name ',name)))
       (setf ,@(loop :for (attribute value) :in body-form
                     :appending `((u:href (attributes ,texprof) ,attribute)
                                  ,value)))
       ,texprof)))

(defmacro define-texture-profile (name &body body)
  "Define a set of attribute defaults that can be applied while defining a
texture."
  (u:with-gensyms (profile)
    `(let ((,profile ,(parse-texture-profile name body)))
       (setf (u:href c::=meta/texture-profiles= (name ,profile)) ,profile))))
