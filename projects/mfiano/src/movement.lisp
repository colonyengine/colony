(in-package :fl.mfiano)

(define-component simple-movement ()
  ((transform :default nil)))

(defmethod initialize-component ((component simple-movement) (context context))
  (with-accessors ((actor actor) (transform transform)) component
    (setf transform (actor-component-by-type actor 'transform))))

(defmethod update-component ((component simple-movement) (context context))
  (with-slots (%transform) component
    (au:mvlet* ((lx ly (get-gamepad-analog context '(:gamepad1 :left-stick)))
                (rx ry (get-gamepad-analog context '(:gamepad1 :right-stick))))
      (let ((vec (v3:make lx ly 0)))
        (v3:scale! vec (if (> (v3:magnitude vec) 1) (v3:normalize vec) vec) 150.0)
        (fl.comp:translate %transform (v3:+ (v3:make -400 0 0) vec) :replace-p t))
      (unless (= rx ry 0.0)
        (let* ((angle (atan (- rx) ry))
               ;; keep angle from 0 to 2pi for easier debugging of other things.
               (angle (if (< angle 0)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (fl.comp:rotate %transform (v3:make 0 0 angle) :replace-p t))))))






(define-component shot-mover ()
  ((transform :default nil)
   (velocity :default 0)))

(defmethod initialize-component ((component shot-mover) (context context))
  (setf (transform component)
        (actor-component-by-type (actor component) 'transform)))


(defmethod update-component ((component shot-mover) (context context))
  (fl.comp:translate
   (transform component)
   ;; fly along the forward axis for this shot
   (let* ((a (m4:rotation-axis-to-vec3
              (fl.comp:local (transform component)) :y))
          (b  (v3:normalize a))
          (move-delta (* (velocity component) (frame-time context))))
     (v3:scale b move-delta))))


(define-component shot-emitter ()
  ((emitter-transform :default nil)))

(defmethod initialize-component ((component shot-emitter) (context context))
  (setf (emitter-transform component)
        (actor-component-by-type (actor component) 'transform)))

(defmethod update-component ((component shot-emitter) (context context))
  (when (or (input-enter-p context '(:gamepad1 :right-shoulder))
            (input-enter-p context '(:mouse :mouse-left)))

    (let* ((parent-model (fl.comp:model (emitter-transform component)))
           (parent-translation (m4:translation-to-vec3 parent-model))
           (parent-rotation (from-scaled-mat4 parent-model))

           ;; here is the code to make the new shot.
           (new-actor (%fl::make-actor context :id (au:unique-name 'shot)))
           (transform (make-component 'fl.comp:transform context
                                      ;; here I init the local location/rotation
                                      ;; to semantically match the emitter
                                      ;; actor.
                                      :translation/current parent-translation
                                      :rotation/current parent-rotation))
           (shot-mover (make-component 'shot-mover context
                                       :velocity 1000))
           (sprite (make-component 'sprite-sheet context
                                   :spec-path '(:local "data/sprites.sexp")
                                   :material 'fl.mfiano.materials::sprite
                                   :animations (make-sprite-sheet-animations
                                                0 0 #(#(.25
                                                        "bullet01"
                                                        "bullet02"
                                                        ))))))

      (attach-multiple-components new-actor transform shot-mover sprite)

      ;; The shot is free in the universe.
      (spawn-actor new-actor context)

      ;; This is the method for destroying actors and components. Add to public
      ;; API. Don't use :ttl in the make-actor call yet.
      (%fl::destroy new-actor context :ttl 1))))






;; TODO: This should go into gamebox-math. It is an alternate form of
;; QUAT:FROM-MAT4 that can handle non-orthonormal rotation matricies.
;; Both should exist, and the user can use what they desire.


(declaim (ftype (function (quat:quat m4:matrix) quat:quat) from-mat4!))
(defun from-scaled-mat4! (out matrix)
  "Convert scaled MATRIX to a quaternion, storing the result in the existing quaternion, OUT."
  (quat:with-components ((q out))

    (m4:with-components ((m matrix))

      ;; Normalize the basis vectors, but don't allocate vector memory,
      ;; and don't alter the original input matrix.
      (let* ((x-rot-denom (sqrt (+ (* m00 m00) (* m10 m10) (* m20 m20))))
             (y-rot-denom (sqrt (+ (* m01 m01) (* m11 m11) (* m21 m21))))
             (z-rot-denom (sqrt (+ (* m02 m02) (* m12 m12) (* m22 m22))))
             ;; normalize x-rotation basis vector
             (nm00 (/ m00 x-rot-denom))
             (nm10 (/ m10 x-rot-denom))
             (nm20 (/ m20 x-rot-denom))
             ;; normalize y-rotation basis vector
             (nm01 (/ m01 y-rot-denom))
             (nm11 (/ m11 y-rot-denom))
             (nm21 (/ m21 y-rot-denom))
             ;; normalize z-rotation basis vector
             (nm02 (/ m02 z-rot-denom))
             (nm12 (/ m12 z-rot-denom))
             (nm22 (/ m22 z-rot-denom)))

        ;; Use the newly normalized values.
        (let ((trace (cl:+ nm00 nm11 nm22 m33))
              (col1 (1+ (cl:- nm00 nm11 nm22)))
              (col2 (1+ (cl:- nm11 nm00 nm22)))
              (col3 (1+ (cl:- nm22 nm00 nm11)))
              (s 0.0f0))
          (cond
            ((plusp trace)
             (setf s (/ 0.5f0 (sqrt trace))
                   qw (/ 0.25f0 s)
                   qx (cl:* (cl:- nm21 nm12) s)
                   qy (cl:* (cl:- nm02 nm20) s)
                   qz (cl:* (cl:- nm10 nm01) s)))
            ((and (>= col1 col2) (>= col1 col3))
             (setf s (/ 0.5f0 (sqrt col1))
                   qw (cl:* (cl:- nm21 nm12) s)
                   qx (/ 0.25f0 s)
                   qy (cl:* (cl:+ nm10 nm01) s)
                   qz (cl:* (cl:+ nm02 nm20) s)))
            ((and (>= col2 col1) (>= col2 col3))
             (setf s (/ 0.5f0 (sqrt col2))
                   qw (cl:* (cl:- nm02 nm20) s)
                   qx (cl:* (cl:+ nm01 nm10) s)
                   qy (/ 0.25f0 s)
                   qz (cl:* (cl:+ nm12 nm21) s)))
            (t
             (setf s (/ 0.5f0 (sqrt col3))
                   qw (cl:* (cl:- nm10 nm01) s)
                   qx (cl:* (cl:+ nm02 nm20) s)
                   qy (cl:* (cl:+ nm12 nm21) s)
                   qz (/ 0.25f0 s)))))))
    out))

(declaim (inline from-mat4))
(declaim (ftype (function (m4:matrix) quat:quat) from-mat4))
(defun from-scaled-mat4 (matrix)
  "Convert scaled MATRIX to a quaternion, storing the result in a freshly allocated quaternion."
  (from-scaled-mat4! (quat:id) matrix))
