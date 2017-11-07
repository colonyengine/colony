(in-package :gear-example)

(define-component mesh-renderer ()
  (vao-id 0)
  (vbo-id 0))


(defmethod initialize-component ((component mesh-renderer) context)
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer))
        ;; define a tetured square centered about origin.
        (glverts (float-vector->static-vector
                  ;; position color uv-coord
                  #(-1.0 -1.0 0.0   1.0 1.0 1.0 1.0   0.0 0.0
                    1.0 1.0 0.0     1.0 1.0 1.0 1.0   1.0 1.0
                    -1.0 1.0 0.0    0.0 1.0

                    -1.0 -1.0 0.0   1.0 1.0 1.0 1.0   0.0 0.0
                    1.0 -1.0 0.0    1.0 1.0 1.0 1.0   1.0 0.0
                    1.0 1.0 0.0     1.0 1.0 1.0 1.0   1.0 1.0
                    )))
        )

    (setf (vao-id component) vao)
    (setf (vbo-id component) vbo)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Bind the VAO
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (gl:bind-vertex-array (vao-id component))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; bind interleaved position/uv datastore vbo into vao here
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (gl:bind-buffer :array-buffer (vbo-id component))

    ;; position/uv data -> GPU
    (%gl:buffer-data :array-buffer
                     (* (length glverts) (gl-type->byte-size :float))
                     (static-vectors:static-vector-pointer glverts)
                     :static-draw)

    ;; Free the memory, don't need it anymore.
    (static-vectors:free-static-vector glverts)
    (setf glverts nil)

    ;; TODO bind layout to shader, but don't know how interface works yet.

    ;; unbind-vao
    (gl:bind-vertex-array 0)))

(defmethod render-component ((component mesh-renderer) context)

  (gl:bind-vertex-array (vao-id component))

  ;; Just use draw-arrays for now.
  (gl:draw-arrays :triangles 0 6))


;; piles of helper functions. This is a horrible place for them.

(defun load-bmp-to-gl-texture (path)
  (let ((sdl-surf (sdl2:load-bmp path))
        (gltex (gl:gen-texture)))
    (gl:bind-texture :texture-2d gltex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-image-2d :texture-2d 0 :rgba
                     (sdl2:surface-width sdl-surf)
                     (sdl2:surface-height sdl-surf)
                     0 :rgba :unsigned-byte (sdl2:surface-pixels sdl-surf))
    (format t "Loaded image ~A (h: ~A, w: ~A)~%"
            path (sdl2:surface-width sdl-surf) (sdl2:surface-height sdl-surf))
    (sdl2:free-surface sdl-surf)
    gltex))


(defun gl-type->cl-type (gl-type)
  (ecase gl-type
    (:float 'single-float)
    (:byte '(integer 0 255))
    (:unsigned-byte '(integer -128 127))
    (:short '(integer -32768 32767))
    (:unsigned-short '(integer 0 65535))
    (:signed-int '(integer −2147483648 2147483647))
    (:unsigned-int '(integer 0 4294967295))
    (:fixed '(integer 0 65535)) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float '(integer 0 65535)) ;; half float in 16 bits of space.
    (:double 'double-float)))

(defun gl-type->byte-size (gl-type)
  (ecase gl-type
    (:float 4)
    (:byte 1)
    (:unsigned-byte 1)
    (:short 2)
    (:unsigned-short 2)
    (:signed-int 4)
    (:unsigned-int 4)
    (:fixed 2) ;; 16.16 fixed point integer in 32-bits of space
    (:half-float 2) ;; half float in 16 bits of space.
    (:double 8)))


(defun vector->static-vector (vec gl-type)
  (let* ((cl-type (gl-type->cl-type gl-type))
         (sv (static-vectors:make-static-vector
              (length vec) :element-type (gl-type->cl-type gl-type))))
    (dotimes (i (length vec))
      (setf (aref sv i) (coerce (aref vec i) cl-type)))
    sv))


(defun byte-vector->static-vector (vec)
  (vector->static-vector vec :byte))

(defun ubyte-vector->static-vector (vec)
  (vector->static-vector vec :unsigned-byte))

(defun short-vector->static-vector (vec)
  (vector->static-vector vec :short))

(defun ushort-vector->static-vector (vec)
  (vector->static-vector vec :unsigned-short))

(defun int-vector->static-vector (vec)
  (vector->static-vector vec :int))

(defun uint-vector->static-vector (vec)
  (vector->static-vector vec :unsigned-int))

(defun float-vector->static-vector (vec)
  (vector->static-vector vec :float))

(defun double-vector->static-vector (vec)
  (vector->static-vector vec :double))
