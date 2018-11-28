(in-package #:fl.example)

(defmethod prologue (context)
  (format t "Running PROLOGUE method!~%")

  (example-0/unrealized-textures context))


(defun example-0/unrealized-textures (context)
  ;; Example 0: Unrealized Procedural Textures.
  ;; These are textures whose texture descriptors are :procedural and
  ;; also referenced in a define-material.
  ;; Goal:
  ;; A) Get the texture-descriptor for ALL unrealized textures.
  ;; For each unrealized texture:
  ;;   B) Set the opengl parameters
  ;;   C) Allocate main memory for the texture.
  ;;   D) Generate a 2d texture into that main memory.
  ;;   E) Upload it to the GPU.
  ;;   F) Free the main memory texture.
  ;;   G) Remove texture from unrealized list.
  (let ((unrealized-textures (%fl::get-unrealized-textures context)))
    (format T "Found unrealized textures: ~A~%" unrealized-textures)

    (dolist (texture unrealized-textures)
      (format t "Processing unrealized texture: ~A~%" (%fl::name texture))
      (cond
        ((eq (first (%fl::name texture))
             'fl.example.textures::texture-test/marble)
         (procedurally-generate-marble context texture))))

    nil))

(defun internal-format-size-in-bytes (internal-format)
  (ecase internal-format
    (:rgba8 4)
    (:rgb8 3)))

(defun procedurally-generate-marble (context texture)
  (format t "  Procedurally generating marble texture! This may take a while...~%")

  ;; TODO: Generate computed texture-descriptor.
  ;; Make a base computed texture descriptor...
  (%fl::generate-computed-texture-descriptor texture)

  ;; Now fill in the texture-type for the computed texdesc.
  (setf (%fl::texture-type (%fl::computed-texdesc texture)) :texture-2d)

  ;; Let opengl know we're using it so we can set things about it.
  (gl:bind-texture (%fl::texture-type (%fl::computed-texdesc texture))
                   (%fl::texid texture))

  ;; Configure the parameters for this texture from profiles or overrides we
  ;; specifically did in the computed-texdesc.
  (%fl::set-opengl-texture-parameters texture)

  ;; TODO: Maybe make some with-attributes macro?
  ;; TODO: Maybe make the attributes stuff a CLOS object too...
  ;; Generate the data we need for this texture.
  (let* ((texture-type
           (%fl::texture-type (%fl::computed-texdesc texture)))
         (width
           (%fl::get-computed-applied-attribute texture :width))
         (height
           (%fl::get-computed-applied-attribute texture :height))
         (internal-format
           (%fl::get-computed-applied-attribute texture :internal-format))
         (pixel-size-in-bytes
           (internal-format-size-in-bytes internal-format))
         (pixel-format
           (%fl::get-computed-applied-attribute texture :pixel-format))
         (pixel-type
           (%fl::get-computed-applied-attribute texture :pixel-type)))

    (let ((data (make-array (* width height pixel-size-in-bytes)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))

      ;; Cheapy pixel/component referencing.
      (flet ((pixel-ref (w h c)
               ;; TODO: C is incorrectly dealt with, we assume byte width.
               (+ (* h (* width pixel-size-in-bytes))
                  (* w pixel-size-in-bytes)
                  c)))

        ;; Whip up a marble texture.
        (loop :for h :below height :do
          (loop :for w :below width :do
            (let* ((noise-value
                     (+ (* w 1/80)
                        (* h 1/100)
                        (* 10 (black-tie::fbm2d (* w 1/400) (* h 1/400)
                                                :octaves 6))))
                   (value-0/1 ;; in range of 0.0 to 1.0
                     (* (abs (sin noise-value))))

                   ;; Generate a decent marble color.
                   (red (floor (* value-0/1 250)))
                   (green (floor (* value-0/1 230)))
                   (blue (floor (* value-0/1 235))))

              (setf (aref data (pixel-ref w h 0)) red
                    (aref data (pixel-ref w h 1)) green
                    (aref data (pixel-ref w h 2)) blue
                    (aref data (pixel-ref w h 3)) 255))))

        ;; TODO: Need to figure out API for allocation for storage or actual
        ;; texture. Possibly break up load-texture-data some more so we can
        ;; use pieces of it here.
        (gl:tex-image-2d texture-type 0 internal-format
                         width height 0
                         pixel-format pixel-type data)

        ;; TODO: generate mipmaps (but make it so we can do this manually if
        ;; needed).
        (gl:generate-mipmap :texture-2d)

        ;; All done, so remove the fact we need to process this.
        (%fl::remove-unrealized-texture texture context)))))
