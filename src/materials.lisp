(in-package :fl.core)

;;; TODO: figure out how to deal with uniform arrays. THey are partially
;;; implemented currently.

;; Held in core-state, the material database for all materials everywhere.
(defclass materials-table ()
  ((%material-table :reader material-table
                    :initarg :material-table
                    :initform (au:dict #'eq))))

;;; Internal Materials-table API
(defun %make-materials-table (&rest init-args)
  (apply #'make-instance 'materials-table init-args))

(defun %lookup-material (material-name core-state)
  "Find a material by its ID in CORE-STATE and return a gethash-like values.
If the material isn't there, return the 'fl.materials:missing-material.
The return value is two values, the first is a material instance, and the
second is T if the material being looked up was actually found, or NIL if it
wasn't (and the missing material used)."
  (symbol-macrolet ((table (material-table (materials core-state))))
    (au:if-found (material (au:href table material-name))
                 material
                 (au:href table (au:ensure-symbol 'missing-material
                                                  'fl.materials)))))

(defun %add-material (material core-state)
  "Add the MATERIAL by its id into CORE-STATE."
  (setf (au:href (material-table (materials core-state)) (id material)) material))

(defun %remove-material (material core-state)
  "Remove the MATERIAL by its id from CORE-STATE."
  (remhash (id material) (material-table (materials core-state))))

(defun %map-materials (func core-state)
  "Map the function FUNC, which expects a material, across all materials in
CORE-STATE. Return a list of the return values of the FUNC."
  (let ((results ()))
    (au:do-hash-values (v (material-table (materials core-state)))
      (push (funcall func v) results))
    (nreverse results)))

;; export PUBLIC API
(defun lookup-material (id context)
  (%lookup-material id (core-state context)))

;;; The value of a uniform or block designation is one of these values.
;;; It holds the original semantic value and any transformation of it that
;;; is actually the usable material value.
(defclass material-value ()
  (;; This is the semantic value for a uniform. In the case of a :sampler-2d
   ;; it is a string to a texture found on disk, etc.
   (%semantic-value :accessor semantic-value
                    :initarg :semantic-value)
   ;; A function the user supplies that performs some a-prioi conversion of
   ;; the semantic value to something apprpropriate for the next stages of
   ;; conversion. This will be added to the below composition chain at the
   ;; right time to form the complete transform of the semantic to the computed
   ;; value. The funciton in this slot is always FIRST in the composition.
   (%semantic-transformer :reader semantic-transformer
                          :initarg :semantic-transformer)

   ;; This is a composition of functions, stored as a list, where the first
   ;; function on the left's result is passed to the next function until the
   ;; end. The last function should be the final converter which produces
   ;; something suitable for the computed-value.
   (%semantic->computed :accessor semantic->computed
                        :initarg :semantic->computed
                        :initform ())

   ;; This is the processed value that is suitable to bind to a uniform.
   (%computed-value :accessor computed-value
                    :initarg :computed-value)

   ;; The function that knows how to bind this value to a shader.
   (%binder :accessor binder
            :initarg :binder)))

(defun %make-material-value (&rest init-args)
  (apply #'make-instance 'material-value init-args))

(defun %deep-copy-material-value (material-value)
  (%make-material-value
   :semantic-value (semantic-value material-value)
   :semantic-transformer (semantic-transformer material-value)
   :semantic->computed (semantic->computed material-value)
   :computed-value (computed-value material-value)
   ;; TODO: Carefully check if there are closures in this binder function
   ;; that I need to deal with in a better manner.
   :binder (binder material-value)))

(defclass material ()
  ((%id :reader id
        :initarg :id)
   ;; This backreference simplifies when we need to change the texture at
   ;; runtime or do something else that makes us grovel around in the
   ;; core-state.
   (%core-state :reader core-state
                :initarg :core-state)
   ;; This is the shader NAME
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is material-value
              :initform (au:dict #'eq))
   (%blocks :reader blocks
            :initarg :blocks
            ;; key is a block keyword, value is material-value
            :initform (au:dict #'eq))
   (%active-texture-unit :accessor active-texture-unit
                         :initarg :active-texture-unit
                         :initform 0)))


(defun %make-material (id shader core-state)
  (make-instance 'material :id id
                           :shader shader
                           :core-state core-state))

(defun %deep-copy-material (current-mat new-mat-name &key (error-p t)
                                                       (error-value nil))

  (when (au:href (material-table (materials (core-state current-mat)))
                 new-mat-name)
    (if error-p
        (error "Cannot copy the material ~A to new name ~A because the new name already exists!"
               (id current-mat) new-mat-name)
        (return-from %deep-copy-material error-value)))

  (let* ((new-id (id current-mat))
         (new-shader (shader current-mat))
         (new-uniforms (au:dict #'eq))
         (new-blocks (au:dict #'eq))
         (new-active-texture-unit (active-texture-unit current-mat))
         (new-mat
           ;; TODO: Fix %make-material so I don't have to do this.
           (make-instance 'material
                          :id new-id
                          :shader new-shader
                          :core-state (core-state current-mat)
                          :uniforms new-uniforms
                          :blocks new-blocks
                          :active-texture-unit new-active-texture-unit)))
    ;; Now we copy over the uniforms
    (maphash
     (lambda (uniform-name material-value)
       (setf (au:href new-uniforms uniform-name)
             (%deep-copy-material-value material-value)))
     (uniforms current-mat))

    ;; Now we compy over the blocks.
    ;; TODO: need to get blocks sorted out as a whole.

    ;; Finally, insert into core-state so everyone can see it.
    (setf (au:href (material-table (materials (core-state current-mat)))
                   new-mat-name)
          new-mat)

    new-mat))

(defun bind-material-uniforms (mat)
  (when mat
    (maphash
     (lambda (uniform-name material-value)
       (funcall (binder material-value) uniform-name (computed-value material-value)))
     (uniforms mat))))

(defun bind-material-buffers (mat)
  (declare (ignore mat))
  ;; TODO
  nil)

;; export PUBLIC API
(defun bind-material (mat)
  (bind-material-uniforms mat)
  (bind-material-buffers mat))

;; export PUBLIC API
;; Todo, these modify the semantic-buffer which then gets processed into a
;; new computed buffer.
(defun mat-uniform-ref (mat uniform-var)
  (let ((material-value (au:href (uniforms mat) uniform-var)))
    (semantic-value material-value)))

;; export PUBLIC API
;; We can only set the semantic-value, which gets automatically upgraded to
;; the computed-value upon setting.
(defun (setf mat-uniform-ref) (new-val mat uniform-var)
  (let ((material-value (au:href (uniforms mat) uniform-var)))
    ;; TODO: Need to do something with the old computed value since it might
    ;; be consuming resources like when it is a sampler on the GPU.

    (setf (semantic-value material-value) new-val)
    (execute-composition/semantic->computed material-value)

    (semantic-value material-value)))

;; export PUBLIC API
;; This is read only, it is the computed value in the material.
(defun mat-computed-uniform-ref (mat uniform-var)
  (let ((material-value (au:href (uniforms mat) uniform-var)))
    (computed-value material-value)))

;; export PUBLIC API
;; current-mat-name must exist.
;; new-mat-name must not exist. :)
(defun copy-material (current-mat new-mat-name &key (error-p t)
                                                 (error-value nil))
  "Copy the material CURRENT-MAT and give the new material the name
NEW-MAT-NAME. Return the new material."
  (%deep-copy-material current-mat new-mat-name
                       :error-p error-p
                       :error-value error-value))



;; Default conversion functions for each uniform type.

(defun gen-sampler/sem->com (core-state)
  "Generates a function that:

Converts the symbolp SEMANTIC-VALUE, which names a define-texture definition to
be used as the source for any sampler type, to a real TEXTURE instance and
return it. The images required by the texture will have been laoded onto the GPU
at the completion of this function."
  (lambda (semantic-value)
    (cond
      ((symbolp semantic-value)
       (rcache-lookup :texture core-state semantic-value))
      (t
       (error "sampler/sem->com: Unable to convert sampler semantic-value: ~A"
              semantic-value)))))


(defun gen-default/sem->com (core-state)
  (declare (ignore core-state))
  (lambda (semantic-value)
    (if (or (stringp semantic-value)
            (arrayp semantic-value)
            (listp semantic-value)
            (vectorp semantic-value))
        (copy-seq semantic-value)
        semantic-value)))




(defun parse-material (name shader uniforms blocks)
  "Return a function which creates a partially complete material instance.
It is partially complete because it does not yet have the shader binder function available for it so
BIND-UNIFORMS cannot yet be called on it."
  `(lambda (core-state)
     (let ((mat (%make-material ',name ,shader core-state)))
       (setf ,@(loop :for (var val . transformer) :in uniforms
                     :append
                     `((au:href (uniforms mat) ,var)
                       (%make-material-value
                        :semantic-value ,val
                        :semantic-transformer
                        (or ,(first transformer) #'identity)))))

       (setf ,@(loop :for (var val . transformer) :in blocks
                     :append
                     `((au:href (blocks mat) ,var)
                       (%make-material-value
                        :semantic-value ,val
                        :semantic-transformer
                        (or ,(first transformer) #'identity)))))

       mat)))

(defun sampler-type->texture-type (sampler-type)
  "Given a SAMPLER-TYPE, like :sampler-2d-array, return the kind of texture-type
that is appropriate for it, such as :texture-2d-array.  Do this for all sampler
types and texture types."

  (au:if-found (texture-type (au:href +sampler-type->texture-type+
                                      sampler-type))
               texture-type
               (error "Unknown sampler-type: ~A~%" sampler-type)))

(defun sampler-p (glsl-type)
  "Return T if the GLSL-TYPE is a sampler like :sampler-2d or :sampler-buffer,
etc. Return NIL otherwise."
  (member glsl-type
          '(:sampler-1d :isampler-1d :usampler-1d
            :sampler-2d :isampler-2d :usampler-2d
            :sampler-3d :isampler-3d :usampler-3d
            :sampler-cube :isampler-cube :usampler-cube
            :sampler-2d-rect :isampler-2d-rect :usampler-2d-rect
            :sampler-1d-array :isampler-1d-array :usampler-1d-array
            :sampler-2d-array :isampler-2d-array :usampler-2d-array
            :sampler-cube-array :isampler-cube-array :usampler-cube-array
            :sampler-buffer :isampler-buffer :usampler-buffer
            :sampler-2d-ms :isampler-2d-ms :usamplers-2d-ms
            :sampler-2d-ms-array :isampler-2d-ms-array :usampler-2d-ms-array)
          :test #'eq))

(defun determine-binder-function (material glsl-type)
  (cond
    ((symbolp glsl-type)
     (if (sampler-p glsl-type)
         (let ((unit (active-texture-unit material)))
           (incf (active-texture-unit material))
           (lambda (uniform-name texture)
             (gl:active-texture unit)
             (gl:bind-texture (sampler-type->texture-type glsl-type)
                              (texid texture))
             (shadow:uniform-int uniform-name unit)))
         (ecase glsl-type
           (:bool #'shadow:uniform-int)
           ((:int :int32) #'shadow:uniform-int)
           (:float #'shadow:uniform-float)
           (:vec2 #'shadow:uniform-vec2)
           (:vec3 #'shadow:uniform-vec3)
           (:vec4 #'shadow:uniform-vec4)
           (:mat2 #'shadow:uniform-mat2)
           (:mat3 #'shadow:uniform-mat3)
           (:mat4 #'shadow:uniform-mat4))))
    ((consp glsl-type)
     (if (sampler-p (first glsl-type))
         ;; TODO: Is this actually correct code for an array of samplers?
         ;; Shouldn't I be binding more than one texture?
         (let ((unit (active-texture-unit material)))
           (incf (active-texture-unit material))
           (lambda (uniform-name texture)
             (gl:active-texture unit)
             (gl:bind-texture (sampler-type->texture-type (first glsl-type))
                              (texid texture))
             (shadow:uniform-int-array uniform-name unit)))
         (ecase (first glsl-type)
           (:bool #'shadow:uniform-int-array)
           ((:int :int32) #'shadow:uniform-int-array)
           (:float #'shadow:uniform-float-array)
           (:vec2 #'shadow:uniform-vec2-array)
           (:vec3 #'shadow:uniform-vec3-array)
           (:vec4 #'shadow:uniform-vec4-array)
           (:mat2 #'shadow:uniform-mat2-array)
           (:mat3 #'shadow:uniform-mat3-array)
           (:mat4 #'shadow:uniform-mat4-array))))
    (t
     (error "Cannot determine binder function for glsl-type: ~S~%"
            glsl-type))))

(defun execute-composition/semantic->computed (material-value)
  (loop :with value = (semantic-value material-value)
        :for transformer :in (semantic->computed material-value)
        :do (setf value (funcall transformer value))
        :finally (setf (computed-value material-value) value)))

(defun annotate-material-uniform (uniform-name material-value
                                  material shader-program core-state)
  (au:if-found (uniform-type-info
                (au:href (shadow:uniforms shader-program) uniform-name))

               (let ((uniform-type (aref uniform-type-info 1)))
                 ;; 1. Find the uniform in the shader-program and get its
                 ;; type-info. Use that to set the binder function.
                 (setf (binder material-value)
                       (determine-binder-function material uniform-type))

                 ;; 2. Figure out the semantic->computed composition function
                 ;; for this specific uniform type. This finally comverts to the
                 ;; computed-value so it must be last in the list.
                 (push (if (sampler-p uniform-type)
                           (gen-sampler/sem->com core-state)
                           (gen-default/sem->com core-state))
                       (semantic->computed material-value))

                 ;; 3. Put the user specified semantic->computed transformer
                 ;; function into the composition function before the current
                 ;; things.
                 (push (semantic-transformer material-value)
                       (semantic->computed material-value))

                 ;; 4. Execute the composition function sequence to produce the
                 ;; computed value.
                 (execute-composition/semantic->computed material-value))

               ;; else
               (error "Material ~s uses unknown uniform ~s in shader ~s. If it is an implicit uniform, those are not supported in materials."
                      (id material) uniform-name (shader material))))

(defun annotate-material-uniforms (material shader-program core-state)
  (maphash
   (lambda (uniform-name material-value)
     (annotate-material-uniform uniform-name material-value
                                material shader-program core-state))
   (uniforms material)))

(defun resolve-all-materials (core-state)
  "Convert all semantic-values to computed-values in the materials. This
must be executed after all the shader programs have been comipiled."
  (%map-materials
   (lambda (material-instance)
     (au:if-found (shader-program (au:href (shaders core-state)
                                           (shader material-instance)))
                  (progn
                    (annotate-material-uniforms material-instance shader-program
                                                core-state)
                    ;; TODO, process blocks here, when appropriate
                    )
                  (error "Material ~s uses an undefined shader: ~s."
                         (id material-instance) (shader material-instance))))
   core-state))

(defmethod extension-file-type ((extension-type (eql 'materials)))
  "mat")

(defmethod prepare-extension ((extension-type (eql 'materials)) owner path)
  (let ((%temp-materials (au:dict #'eq)))
    (declare (special %temp-materials))
    (flet ((%prepare ()
             (load-extensions extension-type path)
             %temp-materials))
      ;; Transfer all parsed, but unresolved, materials in the %temp-materials
      ;; to the core-state's materials table.
      (maphash
       (lambda (material-name gen-material-func)
         (simple-logger:emit :material.extension.process material-name)
         ;; But first, create the partially resolved material.... we will fully
         ;; resolve it later by type checking the uniforms specified and
         ;; creating the binder annotations for the values. Resolving has to be
         ;; done after the shader programs are built.
         (%add-material (funcall gen-material-func owner) owner))
       (%prepare)))))

(defmacro define-material (name &body (body))
  (destructuring-bind (&key enabled shader uniforms blocks &allow-other-keys) body
    `(let ((func ,(parse-material name shader uniforms blocks)))
       (declare (special %temp-materials))
       ,(when enabled
          `(setf (au:href %temp-materials ',name) func)))))
