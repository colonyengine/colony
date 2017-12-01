(in-package :first-light)

(define-component mesh ()
  (location nil)
  ;; These next two are populated from a shared cache kept by this
  ;; component type
  (layout nil)
  (vao nil))


(defun write-buffer-data (vao vbo vertex-data)
  (let ((data (flatten-numbers vertex-data)))
    (kit.gl.vao:vao-buffer-vector vao vbo data)))

(defun fill-mesh-buffer (mesh buffer-id data)
  (let ((index (gethash buffer-id (buffer-indices (layout mesh)))))
    (write-buffer-data (vao mesh) index data)))

(defun make-vao (context location)
  (multiple-value-bind (buffers layout) (load-mesh context location)

    (let ((vao (make-instance 'kit.gl.vao:vao
                              :type (id layout)
                              :primitive (primitive layout)
                              :vertex-count (length (cadar buffers)))))
      (loop :for (id data) :in buffers
            :for index = (gethash id (buffer-indices layout))
            :do (write-buffer-data vao index data))
      (values vao layout))))

(defmethod initialize-component ((component mesh) (context context))
  (symbol-macrolet ((store (shared-storage context component)))
    (with-accessors ((location location)) component

      (unless location
        (error "A mesh component needs a non-nil LOCATION slot."))

      ;; First, we look up to see if we have a mesh-cache at all, if not
      ;; make it. No threads, so this is safe.
      (unless store
        (format t "Creating mesh-cache.~%")
        (setf store (make-mesh-vao-association-cache)))

      ;; Then, check to see if we have a cache value for this location:
      (multiple-value-bind (cache-value presentp)
          (mesh-vao-association store location)

        (cond
          (presentp
           ;; We do! So set the internal slots in the mesh to the
           ;; cached versions.
           (format t "Found cached mesh with location: ~A~%" location)
           (setf (vao component) (vao cache-value)
                 (layout component) (layout cache-value))
           nil)

          (t
           ;; We don't! So make a vao and a cache value and initialize it
           (multiple-value-bind (new-vao new-layout)
               (make-vao context location)
             (format t "Creating cached mesh with location: ~A~%" location)

             (let ((new-cache-value (make-mesh-vao-association-cache-value
                                     location new-layout new-vao)))
               (setf (mesh-vao-association store location) new-cache-value
                     ;; then set the mesh's slots with the new data.
                     (vao component) (vao new-cache-value)
                     (layout component) (layout new-cache-value))))))))))
