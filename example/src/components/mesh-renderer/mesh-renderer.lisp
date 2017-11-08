(in-package :gear-example)

(kit.gl.vao:defvao mesh ()
  (:interleave ()
               (pos :float 3)
               (color :float 4)
               (uv :float 3)))

(define-component mesh-renderer ()
  (vao nil)
  (transform nil))

(defmethod initialize-component ((component mesh-renderer) context)
  (format t "init mesh renderer~%")
  ;; We auto compute this since we always need it.
  (setf (transform component)
        (get-component 'transform (actor component)))

  (let* ( ;; define a textured (soon) square centered about origin.
         ;; this geometry is of unit size: from edge to edge orthogonally is 1
         ;; unit length, centered at origin.
         (num-verts 6)
         (glverts
           ;;(float-vector->static-vector
           (make-array (* 10 num-verts)
                       :element-type 'single-float
                       :initial-contents
                       ;; winding correct? This is CCW.
                       (list 0.5 0.5 0.0     1.0 1.0 1.0 1.0   1.0 1.0 0.0
                             -0.5 0.5 0.0    1.0 1.0 1.0 1.0   0.0 1.0 0.0
                             -0.5 -0.5 0.0   1.0 1.0 1.0 1.0   0.0 0.0 0.0

                             0.5 0.5 0.0     1.0 1.0 1.0 1.0   1.0 1.0 0.0
                             -0.5 -0.5 0.0   1.0 1.0 1.0 1.0   0.0 0.0 0.0
                             0.5 -0.5 0.0    1.0 1.0 1.0 1.0   1.0 0.0 0.0
                             ))))

    (setf (vao component)
          (make-instance 'kit.gl.vao:vao
                         :type 'mesh
                         :primitive :triangles
                         :vertex-count num-verts))

    (kit.gl.vao:vao-buffer-vector (vao component) 0 glverts)

    ;; TODO: Make texture and upload it
    ))

(defmethod render-component ((component mesh-renderer) context)
  (let* ((shaders (gethash :shader-dict context))
         (camera (gethash :camera context)))

    (when (and shaders camera)
      (let ((model (model (transform component)))
            (view (view camera))
            (projection (projection camera)))

	;;(format t "Actor ~A model ~A~%" (id (actor component)) model)

        ;; shader's layout should match the mesh vao spec.
        (kit.gl.shader:use-program shaders 'gear::unlit-texture)
        (kit.gl.shader:uniform-matrix-1-sv shaders :model model)
        (kit.gl.shader:uniform-matrix-1-sv shaders :view view)
        (kit.gl.shader:uniform-matrix-1-sv shaders :proj projection)

        ;; Draw the mesh
        (kit.gl.vao:vao-draw (vao component))))))




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
