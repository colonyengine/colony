(in-package #:virality.examples.shaders)

;;; Art 1
;;; WIP: A Truchet effect across a quad grid.
;;; TODO: This needs fixing to make the mask UV seamlessly tile.

(define-function art1/hash ((p :vec2))
  (setf p (fract (* p (vec2 385.18692 958.5519)))
        p (+ p (dot p (+ p 42.4112))))
  (fract (* (.x p) (.y p))))

(define-function art1/frag (&uniform
                            (res :vec2)
                            (time :float))
  (let* ((scale 4)
         (uv (* (/ (- (.xy gl-frag-coord) (* res 0.5)) (.y res)) scale))
         (cell-id (floor uv))
         (checker (1- (* (mod (+ (.x cell-id) (.y cell-id)) 2) 2)))
         (hash (art1/hash cell-id))
         (grid-uv (- (fract (if (< hash 0.5) (* uv (vec2 -1 1)) uv)) 0.5))
         (circle-uv (- grid-uv
                       (* (sign (+ (.x grid-uv) (.y grid-uv) 1e-7)) 0.5)))
         (dist (length circle-uv))
         (width 0.2)
         (angle (atan (.x circle-uv) (.y circle-uv)))
         (mask (smoothstep 0.01 -0.01 (- (abs (- dist 0.5)) width)))
         (mask-uv (vec2 (fract (/ angle +half-pi+))
                        (* (abs (- (/ (- dist (- 0.5 width)) (* width 2))
                                   0.5))
                           2)))
         (noise (vec3 (+ (* 0.4 (shd/noise:perlin (* 2 mask-uv)))
                         (* 0.3 (shd/noise:perlin-surflet (* 16 mask-uv))))))
         (noise (* noise (vec3 0.6 0.9 0.7) (+ 0.5 (* 0.5 (vec3 uv 1))))))
    (vec4 (* noise mask) 1)))

(define-shader art1 ()
  (:vertex (shd/tex:unlit/vert-nil mesh-attrs))
  (:fragment (art1/frag)))

;;; Art 2

(define-function art2/check-ray ((distance :float))
  (< distance 1e-3))

(define-function art2/frag (&uniform
                            (res :vec2)
                            (time :float))
  (let* ((rtime (* time 0.5))
         (uv (* (/ (- (.xy gl-frag-coord) (* res 0.5)) (.y res))
                (mat2 (cos rtime) (- (sin rtime)) (sin rtime) (cos rtime))))
         (ray-origin (vec3 0 0 -1))
         (look-at (mix (vec3 0) (vec3 -1 0 -1) (sin (+ (* time 0.5) 0.5))))
         (zoom (mix 0.2 0.7 (+ (* (sin time) 0.5) 0.5)))
         (forward (normalize (- look-at ray-origin)))
         (right (normalize (cross (vec3 0 1 0) forward)))
         (up (cross forward right))
         (center (+ ray-origin (* forward zoom)))
         (intersection (+ (* (.x uv) right)
                          (* (.y uv) up)
                          center))
         (ray-direction (normalize (- intersection ray-origin)))
         (distance-surface 0.0)
         (distance-origin 0.0)
         (point (vec3 0))
         (radius (mix 0.3 0.8 (+ 0.5 (* 0.5 (sin (* time 0.4)))))))

    (dotimes (i 1000)
      (setf point (+ (* ray-direction distance-origin)
                     ray-origin)
            distance-surface (- (- (length (vec2 (1- (length (.xz point)))
                                                 (.y point)))
                                   radius)))
      (when (art2/check-ray distance-surface)
        (break))
      (incf distance-origin distance-surface))

    (let ((color (vec3 0)))
      (when (art2/check-ray distance-surface)
        (let* ((x (+ (atan (.x point) (- (.z point))) (* time 0.4)))
               (y (atan (1- (length (.xz point))) (.y point)))
               (ripples (+ (* (sin (* (+ (* y 60) (- (* x 20))) 3)) 0.5) 0.5))
               (waves (sin (+ (* x 2) (+ (- (* y 6)) (* time 5)))))
               (bands (sin (+ (* x 30) (* y 10))))
               (b1 (smoothstep -0.2 0.2 bands))
               (b2 (smoothstep -0.2 0.2 (- bands 0.5)))
               (noise (vec3 (+ (* 0.4 (shd/noise:perlin (* (vec2 x y) 10)))
                               (* 0.7 (shd/noise:cellular (* (vec2 x y) 50)))
                               (* 0.3 (shd/noise:perlin-surflet
                                       (* (vec2 x y) 200))))))
               (noise (shd/color:color-filter noise
                                              (vec3 (sin x) (sin y) 0.5)
                                              1))
               (blend (+ (max (* b1 (- 1 b2)) (* ripples b2 waves))
                         (* waves 0.5 b2)))
               (blend (mix blend
                           (- 1 blend)
                           (smoothstep -0.3 0.3 (sin (+ (* x 2) time))))))
          (setf color (mix (vec3 blend) noise 0.5))))
      (vec4 color 1))))


(define-shader art2 ()
  (:vertex (shd/tex:unlit/vert-nil mesh-attrs))
  (:fragment (art2/frag)))
