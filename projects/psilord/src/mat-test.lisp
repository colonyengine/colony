(in-package :fl.psilord)

(define-component mat-test ()
  ((mesh-rend :default nil)
   (material-copied-p :default nil)
   (mix-color :default (v4:make 1 1 1 1))
   (color-fuzz-range :default (v4:make .2 .2 .2 0))
   (color-fuzz-delta :default (v4:zero))
   (current-color :default (v4:zero))))

(defmethod initialize-component ((mat-test mat-test) (context context))

  ;; TODO: NOTE! We cannot copy the material here, because we don't know if
  ;; the initialize-component has been run for the mesh-renderer which
  ;; converted the material symbol to the actual material object. And,
  ;; setting up the component dependencies is the wrong solution for this.
  ;; So we do this in UPDATE-COMPONENT once.
  ;;
  ;; We probably need the slot annotators indicating their FL role so we can
  ;; convert the material from a symbol to a true material BEFORE any
  ;; use component protocol is executed on them.

  ;; Find the mesh-renderer on my actor.
  (setf (mesh-rend mat-test)
        (actor-component-by-type (actor mat-test) 'mesh-renderer)))

(defun zero-mean-random (magnitude)
  (if (zerop magnitude)
      magnitude
      (* (if (zerop (random 2)) 1 -1)
         (random magnitude))))

(defmethod update-component ((mat-test mat-test) (context context))
  (with-accessors ((mesh-rend mesh-rend) (material-copied-p material-copied-p)
                   (mix-color mix-color) (color-fuzz-range color-fuzz-range)
                   (color-fuzz-delta color-fuzz-delta)
                   (current-color current-color))
      mat-test

    ;; Copy the material ONCE and replace the one in the mesh-renderer with
    ;; the unique copy.
    (unless material-copied-p
      (let* ((mat (material mesh-rend))
             (new-mat
               (copy-material mat (au:unique-name (symbol-name (id mat))))))
        (setf (material mesh-rend) new-mat))
      (setf material-copied-p t))

    ;; Then make a new fuzz-delta given the fuzz-range
    (v4:with-components ((d color-fuzz-delta) (r color-fuzz-range))
      (setf dx (zero-mean-random rx)
            dy (zero-mean-random ry)
            dz (zero-mean-random rz)
            dw (zero-mean-random rw)))

    ;; compute the new color, and clamp it.
    (v4:clamp! current-color
               (v4:+! current-color mix-color color-fuzz-delta)
               :min 0.0
               :max 1.0)

    ;; now update, in real time, the unique material's uniform
    ;; t
    (setf (mat-uniform-ref (material mesh-rend) :mix-color)
          current-color)))
