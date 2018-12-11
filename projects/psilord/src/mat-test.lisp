(in-package :fl.psilord)

(define-component mat-test ()
  ((mesh-rend :default nil)
   (material-copied-p :default nil)
   (mix-color :default (flm:vec4 1))
   (color-fuzz-range :default (flm:vec4 0.2 0.2 0.2))
   (color-fuzz-delta :default (flm:vec4))
   (current-color :default (flm:vec4))))

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
               (copy-material mat (fl.util:unique-name (symbol-name (id mat))))))
        (setf (material mesh-rend) new-mat))
      (setf material-copied-p t))

    ;; Then make a new fuzz-delta given the fuzz-range
    (flm:with-vec4 ((d color-fuzz-delta) (r color-fuzz-range))
      (setf d.r (zero-mean-random r.r)
            d.g (zero-mean-random r.g)
            d.b (zero-mean-random r.b)
            d.a (zero-mean-random r.a)))

    ;; compute the new color, and clamp it.
    (flm:clamp (flm:+ mix-color color-fuzz-delta current-color)
               0.0
               1.0
               current-color)

    ;; TODO: Make a better test.

    ;; now update, in real time, the unique material's uniform
    #++(setf (mat-uniform-ref (material mesh-rend) :mix-color)
             current-color)))
