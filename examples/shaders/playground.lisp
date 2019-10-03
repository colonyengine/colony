(in-package #:virality.examples.shaders)

;;; Art 1
;;; A Truchet effect across a quad grid.

(defun art1/hash ((p :vec2))
  (let* ((p (fract (* p (vec2 385.18692 958.5519))))
         (p (+ p (dot p (+ p 42.4112)))))
    (fract (* (.x p) (.y p)))))

(defun art1/frag (&uniforms
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
         (mask-uv (vec2 (* (- (abs (fract (- (/ (* angle checker) +half-pi+)
                                             (* time 0.3))))
                              0.5)
                           2.0)
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
;;; Trippy effect that navigates erratically around the inside of a surreal
;;; toroidal disco-like world.

(defun art2/check-ray ((distance :float))
  (< distance 1e-3))

(defun art2/frag (&uniforms
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

;;; Art 3
;;; A variation of Art 2 modified by Peter Keller
;;; Adds some distortion to the torus radius to give it more of an organic look,
;;; and other tweaks.

(defun art3/frag (&uniforms
                  (res :vec2)
                  (time :float))
  (let* ((rtime (* time 0.10))
         (uv (* (/ (- (.xy gl-frag-coord) (* res 0.5)) (.y res))
                (mat2 (cos rtime) (- (sin rtime)) (sin rtime) (cos rtime))))
         (ray-origin (vec3 0 0 -1))
         (look-at (mix (vec3 0) (vec3 -1 0 -1) (sin (+ (* time 0.05) 0.05))))
         (zoom (mix 0.2 0.3 (+ (* (sin (* time .25)) 0.5) 0.5)))
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
         (p (* .4 (shd/noise:perlin (* uv 4))))
         (radius (mix 0.6 0.9 (* (sin (* p 4))
                                 (cos (* p 2))))))

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
               (bands (sin (+ (* x 30) (* y 50))))
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
                           (smoothstep -0.5 0.5 (sin (+ (* x 2) time))))))
          (setf color (mix (vec3 blend) noise 0.5))))
      (vec4 color 1))))

(define-shader art3 ()
  (:vertex (shd/tex:unlit/vert-nil mesh-attrs))
  (:fragment (art3/frag)))

;;; Art 4
;;; A somewhat kaleidoscope-like effect with lots of knobs and whistles as
;;; uniforms. Intended to be used with other functions, such as mixing with a
;;; noise or texture pattern.

(defun art4/hash ((p :vec2))
  (let* ((p (fract (vec3 (* p (vec2 385.18692 958.5519))
                         (* (+ (.x p) (.y p)) 534.3851))))
         (p (+ p (dot p (+ p 42.4112)))))
    (fract p)))

(defun art4/xor ((a :float) (b :float))
  (+ (* a (- 1 b)) (* b (- 1 a))))

(defun art4/frag (&uniforms
                  (res :vec2)
                  (time :float)
                  ;; size of circles - [0, 1]
                  (zoom :float)
                  ;; multiplier for ripple speed - [-inf, inf]
                  (speed :float)
                  ;; strength of the overall effect - [0, 1]
                  (strength :float)
                  ;; randomly color each circle instead of monochrome
                  (colorize :bool)
                  ;; render circle outlines instead of filled
                  (outline :bool)
                  ;; amount of focus/detail - [0, 1]
                  (detail :float))
  (let* ((angle (/ (float pi) 4))
         (s (sin angle))
         (c (cos angle))
         (uv (* (/ (- (.xy gl-frag-coord) (* res 0.5)) (.y res))
                (mat2 c (- s) s c)))
         (cell-size (* uv (mix 100 1 (clamp zoom 0 1))))
         (cell-index (floor cell-size))
         (cell-color (if colorize (art4/hash cell-index) (vec3 1)))
         (grid-uv (- (fract cell-size) 0.5))
         (circle 0.0)
         (detail (clamp detail 0 1))
         (strength (mix 1.5 0.2 (clamp strength 0 1)))
         (speed (* time speed)))
    (dotimes (y 3)
      (dotimes (x 3)
        (let* ((offset (1- (vec2 x y)))
               (cell-origin (length (- grid-uv offset)))
               (distance (* (length (+ cell-index offset)) 0.3))
               (radius (mix strength 1.5 (+ (* (sin (- distance speed)) 0.5)
                                            0.5))))
          (setf circle (art4/xor
                        circle
                        (smoothstep radius (* radius detail) cell-origin))))))
    (let ((color (* cell-color (vec3 (mod circle (if outline 1 2))))))
      (vec4 color 1))))

(define-shader art4 ()
  (:vertex (shd/tex:unlit/vert-nil mesh-attrs))
  (:fragment (art4/frag)))
