(in-package :%fl)

;; Held in core-state, the material database for all materials everywhere.
(defclass materials-table ()
  ((%material-table :reader material-table
                    :initarg :material-table
                    :initform (fl.util:dict #'eq))
   (%profiles :reader profiles
              :initarg :profiles
              :initform (fl.util:dict #'eq))))

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
    (fl.util:if-found (material (fl.util:href table material-name))
                      material
                      (fl.util:href table (fl.util:ensure-symbol 'missing-material
                                                                 'fl.materials)))))

(defun %add-material (material core-state)
  "Add the MATERIAL by its id into CORE-STATE."
  (setf (fl.util:href (material-table (materials core-state)) (id material))
        material))

(defun %remove-material (material core-state)
  "Remove the MATERIAL by its id from CORE-STATE."
  (remhash (id material) (material-table (materials core-state))))

(defun %map-materials (func core-state)
  "Map the function FUNC, which expects a material, across all materials in
CORE-STATE. Return a list of the return values of the FUNC."
  (let ((results ()))
    (fl.util:do-hash-values (v (material-table (materials core-state)))
      (push (funcall func v) results))
    (nreverse results)))

;; export PUBLIC API
(defun lookup-material (id context)
  (%lookup-material id (core-state context)))

(defclass material-value ()
  (;; This is a back reference to the material that owns this
   ;; material-uniform-value.
   (%material :accessor material
              :initarg :material)))

;;; The value of a uniform is this type in the material.
;;; It holds the original semantic value and any transformation of it that
;;; is actually the usable material value.
;;;
;;; NOTE: :initforms are supplied here because if how profiles are implemented.
;;; Basically, we need to be able to copy pre-annotated material values from
;;; profiles BEFORE we annotate them for real when reaolving all the materials.
(defclass material-uniform-value (material-value)
  (
   ;; This is the semantic value for a uniform. In the case of a :sampler-2d
   ;; it is a string to a texture found on disk, etc.
   (%semantic-value :accessor semantic-value
                    :initarg :semantic-value
                    :initform nil)

   ;; A function the user supplies that performs some a-prioi conversion of
   ;; the semantic value to something apprpropriate for the next stages of
   ;; conversion. This will be added to the below composition chain at the
   ;; right time to form the complete transform of the semantic to the computed
   ;; value. The function in this slot is always FIRST in the composition.
   (%transformer :reader transformer
                 :initarg :transformer
                 :initform #'identity/for-material-custom-functions)

   ;; When materials get copied, material-uniform-values get copied, and if the
   ;; user is using some custom semantic value, they need to supply the function
   ;; which will deep copy it.
   (%copier :reader copier
            :initarg :copier
            :initform (lambda (sv context mat)
                        (declare (ignore context mat))
                        sv))

   ;; This is a composition of functions, stored as a list, where the first
   ;; function on the left's result is passed to the next function until the
   ;; end. The last function should be the final converter which produces
   ;; something suitable for the computed-value.
   (%semantic->computed :accessor semantic->computed
                        :initarg :semantic->computed
                        :initform ())

   ;; When we're converting the semantic-value to to computed-value, do we
   ;; attempt to force a copy between the semantic-value and the computed value
   ;; even though they COULD be the same in common circumstances?
   (%force-copy :accessor force-copy
                :initarg :force-copy
                :initform nil)

   ;; This is the final processed value that is suitable (or nearly suitable) to
   ;; bind to a uniform.
   (%computed-value :accessor computed-value
                    :initarg :computed-value
                    :initform nil)

   ;; The function that knows how to bind the computed value to a shader.
   (%binder :accessor binder
            :initarg :binder
            :initform (lambda (uniform-name value)
                        (declare (ignore uniform-name value))))))

(defun %make-material-uniform-value (&rest init-args)
  (apply #'make-instance 'material-uniform-value init-args))

(defun %deep-copy-material-uniform-value (material-uniform-value new-mat)
  (let ((copy-mat-value
          (%make-material-uniform-value
           :material new-mat
           ;; Copier func lambda list is (semantic-value context new-mat)
           :semantic-value (funcall (copier material-uniform-value)
                                    (semantic-value material-uniform-value)
                                    (context (core-state new-mat))
                                    new-mat)
           :transformer (transformer material-uniform-value)
           :copier (copier material-uniform-value)
           :semantic->computed (copy-seq (semantic->computed
                                          material-uniform-value))
           :force-copy (force-copy material-uniform-value)
           :computed-value nil
           :binder (binder material-uniform-value))))

    ;; NOTE: Repair the nil computed-value!
    (execute-composition/semantic->computed copy-mat-value)

    copy-mat-value))




;; Shader interface blocks get a wildly different material-value implementation
;; and semantics.
(defclass material-block-value (material-value)
  (
   ;; Store which block-name this originated from for debugging
   (%block-name :reader block-name
                :initarg :block-name)
   ;; The storage type of the block-alias for which this is a value.
   ;; Useful for error messages.
   (%storage-type :reader storage-type
                  :initarg :storage-type)
   ;; each time I request bindings on the material to be performed, how shall
   ;; I deal with it?
   (%binding-policy :accessor binding-policy
                    :initarg :binding-policy
                    ;; next value can be :repeat, :once, or :manual
                    :initform :repeat)
   ;; The buffer-name to bind with the block-alias for which this is a value.
   (%binding-buffer :accessor binding-buffer
                    :initarg :binding-buffer)
   ;; TODO: Add in range information for range binding

   ;; If the binding-policy is :once, then this slot represents if we did the
   ;; binding work. Whenever this goes nil, and the policy is once, we'll rebind
   ;; the block-alias to the binding-buffer name and set bound-once-p to T
   ;; again.
   (%bound-once-p :accessor bound-once-p
                  :initarg :bound-once-p
                  :initform nil)

   ))

(defun %make-material-block-value (&rest init-args)
  (apply #'make-instance 'material-block-value init-args))

(defun %deep-copy-material-block-value (material-block-value new-mat)
  (%make-material-block-value
   :material new-mat
   :block-name (block-name material-block-value)
   :storage-type (storage-type material-block-value)
   :binding-policy (binding-policy material-block-value)
   :binding-buffer (binding-buffer material-block-value)
   :bound-once-p (bound-once-p material-block-value)))

(defclass material-profile ()
  ((%name :reader name
          :initarg :name)
   (%uniforms :reader uniforms
              :initarg :uniforms
              :initform (fl.util:dict #'eq))
   (%blocks :reader blocks
            :initarg :blocks
            :initform (fl.util:dict #'eq))))

(defun %make-material-profile (&rest init-args)
  (apply #'make-instance 'material-profile init-args))

(defun %add-material-profile (profile core-state)
  (setf (fl.util:href (profiles (materials core-state)) (name profile))
        profile))

(defclass material ()
  ((%id :reader id
        :initarg :id)
   ;; This backreference simplifies when we need to change the texture at
   ;; runtime or do something else that makes us grovel around in the
   ;; core-state.
   (%core-state :reader core-state
                :initarg :core-state)
   ;; This is the name of the shader as its symbol.
   (%shader :reader shader ;; :writer defined below.
            :initarg :shader)
   (%profile-overlay-names :reader profile-overlay-names
                           :initarg :profile-overlay-names
                           :initform nil)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is material-uniform-value
              :initform (fl.util:dict #'eq))
   (%blocks :reader blocks
            :initarg :blocks
            ;; Hash tables:
            ;; key1 = <block-alias>,
            ;; value = material-block-value
            :initform (fl.util:dict #'eq))
   (%active-texture-unit :accessor active-texture-unit
                         :initarg :active-texture-unit
                         :initform 0)))

(defmethod (setf shader) (new-val (mat material))
  ;; If we change the :shader of a material, the new shader must abide by
  ;; the old shader's uniforms and types.
  (with-slots (%shader) mat
    (setf %shader new-val)
    ;; Reinitialize the material with the new shader, ensure it type checks,
    ;; and recompute any material uniform value changes.
    (resolve-material mat (core-state mat)))
  new-val)

(defun %make-material (id shader profiles core-state)
  (make-instance 'material :id id
                           :shader shader
                           :profile-overlay-names profiles
                           :core-state core-state))

(defun %deep-copy-material (current-mat new-mat-name &key (error-p t) (error-value nil))

  (when (fl.util:href (material-table (materials (core-state current-mat))) new-mat-name)
    (if error-p
        (error "Cannot copy the material ~A to new name ~A because the new name already exists!"
               (id current-mat) new-mat-name)
        (return-from %deep-copy-material error-value)))
  (let* ((new-id new-mat-name)
         (new-shader (shader current-mat))
         (new-uniforms (fl.util:dict #'eq))
         (new-blocks (fl.util:dict #'eq))
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
     (lambda (uniform-name material-uniform-value)
       (setf (fl.util:href new-uniforms uniform-name)
             (%deep-copy-material-uniform-value material-uniform-value
                                                new-mat)))
     (uniforms current-mat))

    ;; Now we copy over the blocks.
    (maphash
     (lambda (block-alias-name material-block-value)
       (setf (fl.util:href new-blocks block-alias-name)
             (%deep-copy-material-block-value material-block-value
                                              new-mat)))
     (blocks current-mat))

    ;; Finally, insert into core-state so everyone can see it.
    (%add-material new-mat (core-state new-mat))

    new-mat))

(defun bind-material-uniforms (mat)
  (when mat
    (maphash
     (lambda (uniform-name material-uniform-value)
       (when (functionp (semantic-value material-uniform-value))
         (execute-composition/semantic->computed material-uniform-value))
       (let ((cv (computed-value material-uniform-value)))
         (funcall (binder material-uniform-value) uniform-name cv)))
     (uniforms mat))))

(defun bind-material-buffers (mat)
  (when mat
    (maphash
     (lambda (block-alias-name material-block-value)
       (declare (ignore block-alias-name material-block-value))
       ;; TODO: we probably need to call into core-state buffer binding
       ;; management services to perform the binding right here.
       nil)
     (blocks mat))))

;; export PUBLIC API
(defun bind-material (mat)
  (bind-material-uniforms mat)
  (bind-material-buffers mat))

;; export PUBLIC API
;; Todo, these modify the semantic-buffer which then gets processed into a
;; new computed buffer.
(defun mat-uniform-ref (mat uniform-var)
  (fl.util:if-let ((material-uniform-value (fl.util:href (uniforms mat) uniform-var)))
    (semantic-value material-uniform-value)
    (error "Material ~s does not have the referenced uniform ~s. Please add a uniform to the ~
material, and/or check your material profile settings." (id mat) uniform-var)))

;; export PUBLIC API
;; We can only set the semantic-value, which gets automatically upgraded to
;; the computed-value upon setting.
(defun (setf mat-uniform-ref) (new-val mat uniform-var)
  (fl.util:if-let ((material-uniform-value (fl.util:href (uniforms mat) uniform-var)))
    (progn
      ;; TODO: Need to do something with the old computed value since it might
      ;; be consuming resources like when it is a sampler on the GPU.
      (setf (semantic-value material-uniform-value) new-val)
      (execute-composition/semantic->computed material-uniform-value)
      (semantic-value material-uniform-value))
    (error "Material ~s does not have the referenced uniform ~s. Please add a uniform to the ~
material, and/or check your material profile settings." (id mat) uniform-var)))

;; export PUBLIC API
;; This is read only, it is the computed value in the material.
(defun mat-computed-uniform-ref (mat uniform-var)
  (let ((material-uniform-value (fl.util:href (uniforms mat) uniform-var)))
    (computed-value material-uniform-value)))

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

Returns a function that takes three arguments: SEMANTIC-VALUE, CONTEXT, and MAT.
SEMANTIC-VALUE must be a symbol naming a DEFINE-TEXTURE form, or a vector of
such names. CONTEXT, the context object, and MAT the material that contains this
semantic value are ignored in the returned function. The returned function will
convert the symbolic names to actual TEXTURE objects and load all the images
from the texture into the GPU. It will only do this once per TEXTURE name no
matter how many times this is called to perform a conversion. The result is a
TEXTURE object, or a vector of them corresponding in order to the input."
  (lambda (semantic-value context mat)
    (declare (ignore context mat))
    (cond
      ((or (consp semantic-value)
           (symbolp semantic-value)) ;; a single texture symbolic name
       (rcache-lookup :texture core-state
                      (canonicalize-texture-name semantic-value)))
      ((vectorp semantic-value) ;; an array of texture symbolic names
       (map 'vector (lambda (sv)
                      (rcache-lookup :texture core-state
                                     (canonicalize-texture-name sv)))
            semantic-value))
      (t
       (error "sampler/sem->com: Unable to convert sampler semantic-value: ~A"
              semantic-value)))))


(defun gen-default-copy/sem->com (core-state)
  (declare (ignore core-state))
  (lambda (semantic-value context mat)
    (declare (ignore context mat))
    (copy-thing semantic-value)))

(defun identity/for-material-custom-functions (semval context material)
  "This is effectively IDENTITY in that it returns SEMVAL unchanged, but accepts
and ignores the CONTEXT and MATERIAL arguments."
  (declare (ignore context material))
  semval)

(defun parse-material-uniforms (matvar uniforms)
  `(setf ,@(loop :for (var val . options) :in uniforms
                 :append
                 `((fl.util:href (uniforms ,matvar) ,var)
                   (%make-material-uniform-value
                    :material ,matvar
                    :semantic-value ,val
                    :transformer
                    (or ,(getf options :transformer)
                        #'identity/for-material-custom-functions)
                    :copier
                    (or ,(getf options :copier)
                        #'identity/for-material-custom-functions)
                    ;; force-copy is nil by default.
                    :force-copy ,(getf options :force-copy))))))

(defun parse-material-blocks (matvar blocks)
  `(setf ,@(loop :for form :in blocks
                 :append (let ((block-name (getf form :block-name))
                               (storage-type (getf form :storage-type))
                               (block-alias (getf form :block-alias))
                               (binding-buffer (getf form :binding-buffer))
                               (binding-policy (getf form :binding-policy)))
                           `((fl.util:href (blocks ,matvar) ,block-alias)
                             (%make-material-block-value
                              :material ,matvar
                              :block-name ,block-name
                              :storage-type ,storage-type
                              :binding-policy (or ,binding-policy :repeat)
                              :binding-buffer ,binding-buffer
                              :bound-once-p nil))))))

(defun apply-material-profile-overlays (mat core-state)
  ;; Mix in the profiles, in order specified.
  (when (profile-overlay-names mat)
    ;; convert the overlay names to concrete overlay instances
    (let ((concrete-profiles
            (loop :for po-name :in (profile-overlay-names mat)
                  :collect
                  (let ((inst (fl.util:href (profiles (materials core-state))
                                       po-name)))
                    (unless inst
                      (error "Material profile name: ~S doesn't exist."
                             po-name))
                    inst))))
      ;; Insert the uniforms in the profile, overwriting whatever was
      ;; present for that uniform if it existed in a previous uniform.
      (dolist (concrete-profile concrete-profiles)
        (fl.util:do-hash
            (uniform-name material-value (uniforms concrete-profile))
          (setf (fl.util:href (uniforms mat) uniform-name)
                ;; NOTE: We copy here so there is no shared structure
                ;; between material-values and profiles.
                (%deep-copy-material-uniform-value material-value mat)))))))


(defun parse-material (name shader profiles uniforms blocks)
  "Return a function which creates a partially complete material instance.
It is partially complete because it does not yet have the shader binder function
available for it so BIND-UNIFORMS cannot yet be called on it."
  (let ((matvar (gensym "MATERIAL-")))
    `(lambda (core-state)
       ;; NOTE: This thunk happens after all materials are read and the
       ;; profiles have been loaded into core-state.

       (let ((,matvar (%make-material ',name ',shader ',profiles core-state)))

         ;; First, insert in order the profile overlays for this material.
         (apply-material-profile-overlays ,matvar core-state)

         ;; Then, overlay whatever uniforms and blocks the user specified over
         ;; over the profile supplied information, if any. This must come last.
         ,(parse-material-uniforms matvar uniforms)
         ,(parse-material-blocks matvar blocks)
         ,matvar))))

(defun sampler-type->texture-type (sampler-type)
  "Given a SAMPLER-TYPE, like :sampler-2d-array, return the kind of texture-type
that is appropriate for it, such as :texture-2d-array.  Do this for all sampler
types and texture types."

  (fl.util:if-found (texture-type (fl.util:href +sampler-type->texture-type+
                                                sampler-type))
                    texture-type
                    (error "Unknown sampler-type: ~A~%" sampler-type)))

(defun sampler-p (glsl-type)
  "Return T if the GLSL-TYPE is a sampler like :sampler-2d or :sampler-buffer,
or if it a vector of the same. Return NIL otherwise."
  (let ((raw-type (if (symbolp glsl-type)
                      glsl-type
                      (car glsl-type))))
    (member raw-type
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
            :test #'eq)))

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
           (:vec2 (lambda (uniform value)
                    (shadow:uniform-vec2 uniform (flm:get-array value))))
           (:vec3 (lambda (uniform value)
                    (shadow:uniform-vec3 uniform (flm:get-array value))))
           (:vec4 (lambda (uniform value)
                    (shadow:uniform-vec4 uniform (flm:get-array value))))
           (:mat2 (lambda (uniform value)
                    (shadow:uniform-mat2 uniform (flm:get-array value))))
           (:mat3 (lambda (uniform value)
                    (shadow:uniform-mat3 uniform (flm:get-array value))))
           (:mat4 (lambda (uniform value)
                    (shadow:uniform-mat4 uniform (flm:get-array value)))))))

    ((consp glsl-type)
     (if (sampler-p (car glsl-type))
         (let* ((units
                  (loop :for i :from 0 :below (cdr glsl-type)
                        :collect (prog1 (active-texture-unit material)
                                   (incf (active-texture-unit material)))))
                (units (coerce units 'vector)))
           (lambda (uniform-name texture-array)
             ;; Bind all of the textures to their active units first
             (loop :for texture :across texture-array
                   :for unit :across units
                   :do (gl:active-texture unit)
                       (gl:bind-texture
                        (sampler-type->texture-type (car glsl-type))
                        (texid texture)))
             (shadow:uniform-int-array uniform-name units)))

         (ecase (car glsl-type)
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

(defun execute-composition/semantic->computed (material-uniform-value)
  ;; Execute a compositional sequence of functions whose inputs start with the
  ;; semantic-value and end with the computed-value. If the semantic-value is
  ;; actually a function, then invoke it and get the juice it produced and shove
  ;; it down the pipeline.
  (let ((context (context (core-state (material material-uniform-value))))
        (mat (material material-uniform-value))
        (sv (semantic-value material-uniform-value)))
    (loop :with value = (if (functionp sv)
                            (funcall sv context mat)
                            sv)
          :for transformer :in (semantic->computed material-uniform-value)
          :do (setf value (funcall transformer value context mat))
          :finally (setf (computed-value material-uniform-value) value))))

(defun annotate-material-uniform (uniform-name material-uniform-value
                                  material shader-program core-state)
  (fl.util:if-found (uniform-type-info (fl.util:href (shadow:uniforms shader-program) uniform-name))

                    (let ((uniform-type (fl.util:href uniform-type-info :type)))
                      ;; 1. Find the uniform in the shader-program and get its
                      ;; type-info. Use that to set the binder function.
                      (setf (binder material-uniform-value)
                            (determine-binder-function material uniform-type))

                      ;; 2. Figure out the semantic->computed composition function
                      ;; for this specific uniform type. This will be the last
                      ;; function executed and will convert the in-flight semantic
                      ;; value into a real computed value for use by the binder
                      ;; function.
                      ;;
                      ;; NOTE: We set it to NIL here because if we're changing the
                      ;; shader on a material, we'll push multiple copies of the this
                      ;; sequence into the list when we resolve-material on that copy
                      ;; with the changed shader--which is wrong. This will now do
                      ;; the right thing in any (I believe) situation.
                      (setf (semantic->computed material-uniform-value) nil)
                      (push (if (sampler-p uniform-type)
                                (gen-sampler/sem->com core-state)
                                (if (force-copy material-uniform-value)
                                    (gen-default-copy/sem->com core-state)
                                    #'identity/for-material-custom-functions))
                            (semantic->computed material-uniform-value))

                      ;; 3. Put the user specified semantic transformer
                      ;; function into the composition sequence before the above.
                      (push (transformer material-uniform-value)
                            (semantic->computed material-uniform-value))

                      ;; 4. Execute the composition function sequence to produce the
                      ;; computed value.
                      (execute-composition/semantic->computed
                       material-uniform-value))

                    ;; else
                    (error "Material ~s uses unknown uniform ~s in shader ~s. If it is an implicit ~
uniform, those are not supported in materials."
                           (id material) uniform-name (shader material))))

(defun annotate-material-uniforms (material shader-program core-state)
  (maphash
   (lambda (uniform-name material-uniform-value)
     (annotate-material-uniform uniform-name material-uniform-value
                                material shader-program core-state))
   (uniforms material)))



(defun annotate-material-block (block-alias-name material-block-value
                                material shader-program core-state)

  (declare (ignore shader-program core-state))

  ;; 1. Validate that this material-block-value is present in the shaders
  ;; in core-state
  ;; TODO


  ;; 2. Create the block-name-alias, but only once.
  (unless (shadow:find-block block-alias-name)
    (shadow::create-block-alias (storage-type material-block-value)
                                (block-name material-block-value)
                                (shader material)
                                block-alias-name))
  )

(defun annotate-material-blocks (material shader-program core-state)
  ;; TODO: Ensure that all :block-alias names are unique in the material

  (maphash
   (lambda (block-alias-name material-block-value)
     (annotate-material-block block-alias-name material-block-value
                              material shader-program core-state))
   (blocks material)))


(defun resolve-material (material-instance core-state)
  "Convert semantic-values to computed-values. Type check the uniforms against
the shader program in the material."
  (fl.util:if-found (shader-program (fl.util:href (shaders core-state)
                                                  (shader material-instance)))
                    (progn
                      (annotate-material-uniforms material-instance shader-program
                                                  core-state)
                      (annotate-material-blocks material-instance shader-program
                                                core-state)
                      )
                    (error "Material ~s uses an undefined shader: ~s."
                           (id material-instance) (shader material-instance))))

(defun resolve-all-materials (core-state)
  "Convert all semantic-values to computed-values for all materials. This
must be executed after all the shader programs have been compiled."
  ;; TODO: Check that all :block-alias names are actually unique between
  ;; materials.

  (%map-materials (lambda (mat)
                    (resolve-material mat core-state))
                  core-state))

(defmethod extension-file-type ((extension-type (eql :materials)))
  "mat")


(defmethod prepare-extension ((extension-type (eql :materials)) core-state)
  (let ((%temp-materials (fl.util:dict #'eq))
        (%temp-material-profiles (fl.util:dict #'eq)))
    (declare (special %temp-materials %temp-material-profiles))

    (flet ((%prepare ()
             (map-extensions (context core-state) extension-type)
             (values %temp-material-profiles %temp-materials)))

      (multiple-value-bind (profiles materials) (%prepare)
        ;; The order doesn't matter. we can type check the materials wrt
        ;; profiles after reading _all_ the available materials extensions.
        ;; Process all defined profiles.

        ;; Process all profiles.
        (fl.util:do-hash-values (profile profiles)
          (%add-material-profile profile core-state))

        ;; Process all materials.
        (fl.util:do-hash-values (gen-material-func materials)
          (%add-material (funcall gen-material-func core-state) core-state))))))

(defun parse-material-profile (name uniforms blocks)
  (let ((matprof (gensym "MATERIAL-PROFILE")))
    `(let* ((,matprof (%make-material-profile :name ',name)))
       ,(parse-material-uniforms matprof uniforms)

       ;; TODO: We prevent processing of blocks in material-profiles
       ;; until we discoverifit is a good idea or not.
       ;; ,(parse-material-blocks matprof blocks)
       (when ',blocks
         (error "Interface blocks are not supported in material profiles: ~A"
                ',blocks))
       ,matprof)))

(defmacro define-material-profile (name &body (body))
  "Define a set of uniform and block shader attribute defaults that can be
applied in an overlay manner while defining a material."
  (let ((matprof (gensym "MATPROF")))
    (destructuring-bind (&key uniforms blocks) body
      `(let* ((,matprof ,(parse-material-profile name uniforms blocks)))
         (declare (special %temp-material-profiles))
         (setf (fl.util:href %temp-material-profiles (name ,matprof)) ,matprof)))))

(defmacro define-material (name &body (body))
  ;; TODO: better parsing and type checking of material forms...
  (fl.util:with-unique-names (func)
    (destructuring-bind (&key (enabled t) shader profiles uniforms blocks) body
      `(let ((,func ,(parse-material name shader profiles uniforms blocks)))
         (declare (special %temp-materials))
         ,(when enabled
            `(setf (fl.util:href %temp-materials ',name) ,func))
         (export ',name)))))

(defmacro using-material (material (&rest bindings) &body body)
  (fl.util:with-unique-names (material-ref)
    `(let ((,material-ref ,material))
       (shadow:with-shader-program (shader ,material-ref)
         (setf ,@(loop :for (k v) :on bindings :by #'cddr
                       :collect `(mat-uniform-ref ,material-ref ,k)
                       :collect v))
         (bind-material ,material-ref)
         ,@body))))
