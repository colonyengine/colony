(in-package #:virality.shaders.visualization)

(define-function collider/sphere/vert ()
  (values))

(define-function collider/sphere/geom (&uniform
                                       (model :mat4)
                                       (view :mat4)
                                       (proj :mat4)
                                       (collider-local-position :vec3)
                                       (radius :float))
  (declare (output-primitive :kind :line-strip :max-vertices 256))
  ;; TODO: Ok, it was REALLY EASY to draw the sphere collider like a spiral, but
  ;; it clearly needs a better visualization. I'm thinking a line-strip that
  ;; goes around each plane around the origin. It turns out if you do some math
  ;; you can trace a line strip across the sphere so it cuts it into eights.
  (let ((mvp (* proj view model))
        (iterations 256))
    (dotimes (i iterations)
      (let* ((coeff (/ i (1- (float iterations))))
             (theta (float (mix 0 (* 24 pi) coeff)))
             (phi (float (mix 0 pi coeff)))
             (x (* radius (cos theta) (sin phi)))
             (y (* radius (sin theta) (sin phi)))
             (z (* radius (cos phi)))
             (spiral-world-point (* mvp (vec4 x y z 1))))
        (emit ()
              spiral-world-point)))
    (end-primitive))
  (values))

(define-function collider/sphere/frag (&uniform
                                       (in-contact-color :vec4)
                                       (not-in-contact-color :vec4)
                                       (in-contact-p :bool))
  (if in-contact-p
      in-contact-color
      not-in-contact-color))

(define-shader collider/sphere (:primitive :points)
  (:vertex (collider/sphere/vert))
  (:geometry (collider/sphere/geom))
  (:fragment (collider/sphere/frag)))
