(in-package #:virality.engine)

;; Held in core, the material database for all materials everywhere.
(defclass materials-table ()
  ((%material-table :reader material-table
                    :initarg :material-table
                    :initform (u:dict))
   (%profiles :reader profiles
              :initarg :profiles
              :initform (u:dict))))

;;; Internal Materials-table API
(defun %make-materials-table (&rest init-args)
  (apply #'make-instance 'materials-table init-args))

(defun %lookup-material (material-name core)
  "Find a material by its ID in CORE and return a gethash-like values. If the
material isn't there, return the 'contrib.mat:missing-material. The return value
is two values, the first is a material instance, and the second is T if the
material being looked up was actually found, or NIL if it wasn't (and the
missing material used)."
  (symbol-macrolet ((table (material-table (materials core))))
    (u:if-found (material (u:href table material-name))
                material
                (u:href table (a:ensure-symbol "MISSING-MATERIAL"
                                               :contrib.mat)))))

(defun %add-material (material core)
  "Add the MATERIAL by its id into CORE."
  (setf (u:href (material-table (materials core)) (id material))
        material))

(defun %remove-material (material core)
  "Remove the MATERIAL by its id from CORE."
  (remhash (id material) (material-table (materials core))))

(defun %map-materials (func core)
  "Map the function FUNC, which expects a material, across all materials in
CORE. Return a list of the return values of the FUNC."
  (let (results)
    (u:do-hash-values (v (material-table (materials core)))
      (push (funcall func v) results))
    (nreverse results)))

;; export PUBLIC API
(defun lookup-material (id context)
  (%lookup-material id (core context)))

(defclass material-value ()
  ;; This is a back reference to the material that owns this
  ;; material-uniform-value.
  ((%material :accessor material
              :initarg :material)))

;;; The value of a uniform is this type in the material. It holds the original
;;; semantic value and any transformation of it that is actually the usable
;;; material value.
;;; NOTE: :initforms are supplied here because if how profiles are implemented.
;;; Basically, we need to be able to copy pre-annotated material values from
;;; profiles BEFORE we annotate them for real when reaolving all the materials.

(defclass material-uniform-value (material-value)
  (;; This is the semantic value for a uniform. In the case of a :sampler-2d it
   ;; is a string to a texture found on disk, etc.
   (%semantic-value :accessor semantic-value
                    :initarg :semantic-value
                    :initform nil)
   ;; A function the user supplies that performs some a-prioi conversion of the
   ;; semantic value to something apprpropriate for the next stages of
   ;; conversion. This will be added to the below composition chain at the right
   ;; time to form the complete transform of the semantic to the computed value.
   ;; The function in this slot is always FIRST in the composition.
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
                        :initform nil)
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
            :initform (constantly nil))))

(defun %make-material-uniform-value (&rest init-args)
  (apply #'make-instance 'material-uniform-value init-args))

(defun %deep-copy-material-uniform-value (material-uniform-value new-mat)
  (let ((copy-mat-value
          (%make-material-uniform-value
           :material new-mat
           ;; Copier func lambda list is (semantic-value context new-mat)
           :semantic-value (funcall (copier material-uniform-value)
                                    (semantic-value material-uniform-value)
                                    (context (core new-mat))
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
  ;; Store which block-name this originated from for debugging
  ((%block-name :reader block-name
                :initarg :block-name)
   ;; The storage type of the block-alias for which this is a value. Useful for
   ;; error messages.
   (%storage-type :reader storage-type
                  :initarg :storage-type)
   ;; each time I request bindings on the material to be performed, how shall I
   ;; deal with it?
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
                  :initform nil)))

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
              :initform (u:dict))
   (%blocks :reader blocks
            :initarg :blocks
            :initform (u:dict))))

(defun %make-material-profile (&rest init-args)
  (apply #'make-instance 'material-profile init-args))

(defun %add-material-profile (profile core)
  (setf (u:href (profiles (materials core)) (name profile)) profile))

(defclass material ()
  ((%id :reader id
        :initarg :id)
   ;; This backreference simplifies when we need to change the texture at
   ;; runtime or do something else that makes us grovel around in the core.
   (%core :reader core
          :initarg :core)
   ;; This is the name of the shader as its symbol.
   (%shader :reader shader ;; :writer defined below.
            :initarg :shader)
   (%instances :reader instances
               :initarg :instances)
   (%attributes :reader attributes
                :initarg :attributes
                :initform nil)
   (%profile-overlay-names :reader profile-overlay-names
                           :initarg :profile-overlay-names
                           :initform nil)
   (%uniforms :reader uniforms
              :initarg :uniforms
              ;; key is a uniform keyword, value is material-uniform-value
              :initform (u:dict))
   (%blocks :reader blocks
            :initarg :blocks
            ;; Hash tables:
            ;; key1 = <block-alias>,
            ;; value = material-block-value
            :initform (u:dict))
   (%active-texture-unit :accessor active-texture-unit
                         :initarg :active-texture-unit
                         :initform 0)))

(defmethod (setf shader) (value (mat material))
  ;; If we change the :shader of a material, the new shader must abide by the
  ;; old shader's uniforms and types.
  (with-slots (%shader) mat
    (setf %shader value)
    ;; Reinitialize the material with the new shader, ensure it type checks, and
    ;; recompute any material uniform value changes.
    (resolve-material mat (core mat)))
  value)

(defmethod (setf instances) (value (mat material))
  (with-slots (%instances) mat
    (setf %instances value)))

(defmethod (setf attributes) (value (mat material))
  (with-slots (%attributes) mat
    (setf %attributes value)))

(defun %make-material (id shader instances attributes profiles core)
  (make-instance 'material :id id
                           :shader shader
                           :instances instances
                           :attributes attributes
                           :profile-overlay-names profiles
                           :core core))

(defun %deep-copy-material (current-mat new-mat-name
                            &key (error-p t) (error-value nil))
  (when (u:href (material-table (materials (core current-mat)))
                new-mat-name)
    (if error-p
        (error "Cannot copy the material ~A to new name ~A because the new ~
                name already exists!"
               (id current-mat) new-mat-name)
        (return-from %deep-copy-material error-value)))
  (let* ((new-id new-mat-name)
         (new-shader (shader current-mat))
         (new-instances (instances current-mat))
         (new-attributes (attributes current-mat))
         (new-uniforms (u:dict))
         (new-blocks (u:dict))
         (new-active-texture-unit (active-texture-unit current-mat))
         (new-mat
           ;; TODO: Fix %make-material so I don't have to do this.
           (make-instance 'material
                          :id new-id
                          :shader new-shader
                          :instances new-instances
                          :attributes new-attributes
                          :core (core current-mat)
                          :uniforms new-uniforms
                          :blocks new-blocks
                          :active-texture-unit new-active-texture-unit)))
    ;; Now we copy over the uniforms
    (maphash
     (lambda (uniform-name material-uniform-value)
       (setf (u:href new-uniforms uniform-name)
             (%deep-copy-material-uniform-value material-uniform-value
                                                new-mat)))
     (uniforms current-mat))
    ;; Now we copy over the blocks.
    (maphash
     (lambda (block-alias-name material-block-value)
       (setf (u:href new-blocks block-alias-name)
             (%deep-copy-material-block-value material-block-value new-mat)))
     (blocks current-mat))
    ;; Finally, insert into core so everyone can see it.
    (%add-material new-mat (core new-mat))
    new-mat))

(defun bind-material-uniforms (mat)
  (when mat
    (maphash
     (lambda (uniform-name material-uniform-value)
       (when (functionp (semantic-value material-uniform-value))
         (execute-composition/semantic->computed material-uniform-value))
       (let ((shader (shader (material material-uniform-value)))
             (cv (computed-value material-uniform-value)))
         (funcall (binder material-uniform-value) shader uniform-name cv)))
     (uniforms mat))))

(defun bind-material-buffers (mat)
  (when mat
    (maphash
     (lambda (block-alias-name material-block-value)
       (declare (ignore block-alias-name material-block-value))
       ;; TODO: we probably need to call into core buffer binding management
       ;; services to perform the binding right here.
       nil)
     (blocks mat))))

;; export PUBLIC API
(defun bind-material (mat)
  (bind-material-uniforms mat)
  (bind-material-buffers mat))

;; export PUBLIC API
;; TODO: these modify the semantic-buffer which then gets processed into a new
;; computed buffer.
(defun mat-uniform-ref (mat uniform-var)
  (a:if-let ((material-uniform-value (u:href (uniforms mat) uniform-var)))
    (semantic-value material-uniform-value)
    (error "Material ~s does not have the referenced uniform ~s.~%~
            Please add a uniform to the material, and/or check your material ~
            profile settings."
           (id mat) uniform-var)))

;; export PUBLIC API
;; We can only set the semantic-value, which gets automatically upgraded to the
;; computed-value upon setting.
(defun (setf mat-uniform-ref) (new-val mat uniform-var)
  (a:if-let ((material-uniform-value (u:href (uniforms mat) uniform-var)))
    (progn
      ;; TODO: Need to do something with the old computed value since it might
      ;; be consuming resources like when it is a sampler on the GPU.
      (setf (semantic-value material-uniform-value) new-val)
      (execute-composition/semantic->computed material-uniform-value)
      (semantic-value material-uniform-value))
    (error "Material ~s does not have the referenced uniform ~s.~%~
            Please add a uniform to the material, and/or check your material ~
            profile settings."
           (id mat) uniform-var)))

;; export PUBLIC API
;; This is read only, it is the computed value in the material.
(defun mat-computed-uniform-ref (mat uniform-var)
  (let ((material-uniform-value (u:href (uniforms mat) uniform-var)))
    (computed-value material-uniform-value)))

;; export PUBLIC API
;; current-mat-name must exist.
;; new-mat-name must not exist. :)
(defun copy-material (current-mat new-mat-name
                      &key (error-p t) (error-value nil))
  "Copy the material CURRENT-MAT and give the new material the name
NEW-MAT-NAME. Return the new material."
  (%deep-copy-material
   current-mat new-mat-name :error-p error-p :error-value error-value))

;;; Default conversion functions for each uniform type.

(defun gen-sampler/sem->com ()
  "Generates a function that: Returns a function that takes three arguments:
SEMANTIC-VALUE, CONTEXT, and MAT. SEMANTIC-VALUE must be a symbol naming a
DEFINE-TEXTURE form, or a vector of such names. CONTEXT, the context object, and
MAT the material that contains this semantic value are ignored in the returned
function. The returned function will convert the symbolic names to actual
TEXTURE objects and load all the images from the texture into the GPU. It will
only do this once per TEXTURE name no matter how many times this is called to
perform a conversion. The result is a TEXTURE object, or a vector of them
corresponding in order to the input."
  (lambda (semantic-value context mat)
    (declare (ignore mat))
    (etypecase semantic-value
      ((or cons symbol)
       (rcache-lookup context
                      :texture
                      (canonicalize-texture-name semantic-value)))
      (vector
       (map 'vector
            (lambda (sv)
              (rcache-lookup context :texture (canonicalize-texture-name sv)))
            semantic-value)))))

(defun gen-default-copy/sem->com ()
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
                 `((u:href (uniforms ,matvar) ,var)
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
                           `((u:href (blocks ,matvar) ,block-alias)
                             (%make-material-block-value
                              :material ,matvar
                              :block-name ,block-name
                              :storage-type ,storage-type
                              :binding-policy (or ,binding-policy :repeat)
                              :binding-buffer ,binding-buffer
                              :bound-once-p nil))))))

(defun apply-material-profile-overlays (mat core)
  ;; Mix in the profiles, in order specified.
  (when (profile-overlay-names mat)
    ;; convert the overlay names to concrete overlay instances
    (let ((concrete-profiles
            (loop :for po-name :in (profile-overlay-names mat)
                  :for inst = (u:href (profiles (materials core)) po-name)
                  :if inst
                    :collect inst
                  :else
                    :do (error "Material profile name: ~S doesn't exist."
                               po-name))))
      ;; Insert the uniforms in the profile, overwriting whatever was present
      ;; for that uniform if it existed in a previous uniform.
      (dolist (concrete-profile concrete-profiles)
        (u:do-hash (uniform-name material-value (uniforms concrete-profile))
          (setf (u:href (uniforms mat) uniform-name)
                ;; NOTE: We copy here so there is no shared structure between
                ;; material-values and profiles.
                (%deep-copy-material-uniform-value material-value mat)))))))

(defun parse-material (name shader instances attributes profiles uniforms
                       blocks)
  "Return a function which creates a partially complete material instance.
It is partially complete because it does not yet have the shader binder function
available for it so BIND-UNIFORMS cannot yet be called on it."
  (a:with-gensyms (matvar)
    `(lambda (core)
       (let ((,matvar (%make-material ',name ',shader ,instances ',attributes
                                      ',profiles core)))
         ;; First, insert in order the profile overlays for this material.
         (apply-material-profile-overlays ,matvar core)
         ;; Then, overlay whatever uniforms and blocks the user specified over
         ;; over the profile supplied information, if any. This must come last.
         ,(parse-material-uniforms matvar uniforms)
         ,(parse-material-blocks matvar blocks)
         ,matvar))))

(defun sampler-type->texture-type (sampler-type)
  "Given a SAMPLER-TYPE, like :sampler-2d-array, return the kind of texture-type
that is appropriate for it, such as :texture-2d-array. Do this for all sampler
types and texture types."
  (u:if-found (texture-type (u:href +sampler-type->texture-type+ sampler-type))
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
  (typecase glsl-type
    (symbol
     (if (sampler-p glsl-type)
         (let ((unit (active-texture-unit material)))
           (incf (active-texture-unit material))
           (lambda (shader uniform-name texture)
             (gl:active-texture unit)
             (gl:bind-texture (sampler-type->texture-type glsl-type)
                              (texid texture))
             (gpu:uniform-int shader uniform-name unit)))
         (ecase glsl-type
           (:bool (lambda (shader uniform value)
                    (gpu:uniform-int shader uniform (if value 1 0))))
           (:int #'gpu:uniform-int)
           (:float #'gpu:uniform-float)
           (:vec2 #'gpu:uniform-vec2)
           (:vec3 #'gpu:uniform-vec3)
           (:vec4 #'gpu:uniform-vec4)
           (:mat2 #'gpu:uniform-mat2)
           (:mat3 #'gpu:uniform-mat3)
           (:mat4 #'gpu:uniform-mat4))))
    (cons
     (if (sampler-p (car glsl-type))
         (let* ((units
                  (loop :for i :from 0 :below (cdr glsl-type)
                        :collect (prog1 (active-texture-unit material)
                                   (incf (active-texture-unit material)))))
                (units (coerce units 'vector)))
           (lambda (shader uniform-name texture-array)
             ;; Bind all of the textures to their active units first
             (loop :for texture :across texture-array
                   :for unit :across units
                   :do (gl:active-texture unit)
                       (gl:bind-texture
                        (sampler-type->texture-type (car glsl-type))
                        (texid texture)))
             (gpu:uniform-int-array shader uniform-name units)))
         (ecase (car glsl-type)
           (:bool #'gpu:uniform-int-array)
           (:int #'gpu:uniform-int-array)
           (:float #'gpu:uniform-float-array)
           (:vec2 #'gpu:uniform-vec2-array)
           (:vec3 #'gpu:uniform-vec3-array)
           (:vec4 #'gpu:uniform-vec4-array)
           (:mat2 #'gpu:uniform-mat2-array)
           (:mat3 #'gpu:uniform-mat3-array)
           (:mat4 #'gpu:uniform-mat4-array))))
    (t
     (error "Cannot determine binder function for glsl-type: ~S~%"
            glsl-type))))

(defun execute-composition/semantic->computed (material-uniform-value)
  ;; Execute a compositional sequence of functions whose inputs start with the
  ;; semantic-value and end with the computed-value. If the semantic-value is
  ;; actually a function, then invoke it and get the juice it produced and shove
  ;; it down the pipeline.
  (let ((context (context (core (material material-uniform-value))))
        (mat (material material-uniform-value))
        (sv (semantic-value material-uniform-value)))
    (loop :with value = (if (functionp sv)
                            (funcall sv context mat)
                            sv)
          :for transformer :in (semantic->computed material-uniform-value)
          :do (setf value (funcall transformer value context mat))
          :finally (setf (computed-value material-uniform-value) value))))

(defun annotate-material-uniform (uniform-name uniform-value material
                                  shader-program)
  (u:if-found (type-info (u:href (gpu:uniforms shader-program)
                                 uniform-name))
              (let ((uniform-type (u:href type-info :type)))
                ;; 1. Find the uniform in the shader-program and get its
                ;; type-info. Use that to set the binder function.
                (setf (binder uniform-value) (determine-binder-function
                                              material uniform-type))
                ;; 2. Figure out the semantic->computed composition function
                ;; for this specific uniform type. This will be the last
                ;; function executed and will convert the in-flight semantic
                ;; value into a real computed value for use by the binder
                ;; function.
                ;; NOTE: We set it to NIL here because if we're changing the
                ;; shader on a material, we'll push multiple copies of the this
                ;; sequence into the list when we resolve-material on that copy
                ;; with the changed shader--which is wrong. This will now do
                ;; the right thing in any (I believe) situation.
                (setf (semantic->computed uniform-value) nil)
                (push (if (sampler-p uniform-type)
                          (gen-sampler/sem->com)
                          (if (force-copy uniform-value)
                              (gen-default-copy/sem->com)
                              #'identity/for-material-custom-functions))
                      (semantic->computed uniform-value))
                ;; 3. Put the user specified semantic transformer function into
                ;; the composition sequence before the above.
                (push (transformer uniform-value)
                      (semantic->computed uniform-value))
                ;; 4. Execute the composition function sequence to produce the
                ;; computed value.
                (execute-composition/semantic->computed uniform-value))
              (error "Material ~s uses unknown uniform ~s in shader ~s."
                     (id material) uniform-name (shader material))))

(defun annotate-material-uniforms (material shader-program)
  (maphash
   (lambda (uniform-name uniform-value)
     (annotate-material-uniform
      uniform-name uniform-value material shader-program))
   (uniforms material)))

(defun annotate-material-block (alias-name block-value material shader-program
                                core)
  (declare (ignore shader-program core))
  ;; 1. Validate that this material-block-value is present in the shaders in
  ;; core
  ;; TODO: 2. Create the block-name-alias, but only once.
  (unless (gpu:find-block alias-name)
    (gpu:create-block-alias (storage-type block-value)
                               (block-name block-value)
                               (shader material)
                               alias-name)))

(defun annotate-material-blocks (material shader-program core)
  ;; TODO: Ensure that all :block-alias names are unique in the material
  (maphash
   (lambda (alias-name block-value)
     (annotate-material-block
      alias-name block-value material shader-program core))
   (blocks material)))

(defun resolve-material (material-instance core)
  "Convert semantic-values to computed-values. Type check the uniforms against
the shader program in the material."
  (u:if-found (shader-program (u:href (shaders core)
                                      (shader material-instance)))
              (progn
                (annotate-material-uniforms material-instance shader-program)
                (annotate-material-blocks
                 material-instance shader-program core))
              (error "Material ~s uses an undefined shader: ~s."
                     (id material-instance) (shader material-instance))))

(defun resolve-all-materials (core)
  "Convert all semantic-values to computed-values for all materials. This must
be executed after all the shader programs have been compiled."
  ;; TODO: Check that all :block-alias names are actually unique between
  ;; materials.
  (%map-materials
   (lambda (mat)
     (resolve-material mat core))
   core))

(defun parse-material-profile (name uniforms blocks)
  (a:with-gensyms (matprof)
    `(let* ((,matprof (%make-material-profile :name ',name)))
       ,(parse-material-uniforms matprof uniforms)
       ;; TODO: We prevent processing of blocks in material-profiles until we
       ;; discover if it is a good idea or not.
       ;; ,(parse-material-blocks matprof blocks)
       (when ',blocks
         (error "Interface blocks are not supported in material profiles: ~a"
                ',blocks))
       ,matprof)))

(defmacro define-material-profile (name &body (body))
  "Define a set of uniform and block shader attribute defaults that can be
applied in an overlay manner while defining a material."
  (a:with-gensyms (profile)
    (let ((definition '(meta 'material-profiles)))
      (destructuring-bind (&key uniforms blocks) body
        `(let ((,profile ,(parse-material-profile name uniforms blocks)))
           (unless ,definition
             (setf (meta 'material-profiles) (u:dict)))
           (setf (u:href ,definition (name ,profile)) ,profile))))))

(defmacro define-material (name &body (body))
  ;; TODO: better parsing and type checking of material forms...
  (a:with-gensyms (func)
    (let ((definition '(meta 'materials)))
      (destructuring-bind (&key shader profiles (instances 1) attributes
                             uniforms blocks)
          body
        `(let ((,func ,(parse-material name shader instances attributes profiles
                                       uniforms blocks)))
           (unless ,definition
             (setf (meta 'materials) (u:dict)))
           (setf (u:href ,definition ',name) ,func)
           (export ',name))))))

;; TODO: Make this constant time
(defmacro with-depth-function (material &body body)
  `(destructuring-bind (&key depth) (attributes ,material)
     (if depth
         (let ((old-depth (gl:get-enum :depth-func)))
           (unwind-protect
                (progn
                  (gl:depth-func depth)
                  ,@body)
             (gl:depth-func old-depth)))
         (progn ,@body))))

(defmacro with-material (material (&rest bindings) &body body)
  (a:with-gensyms (material-ref)
    `(let ((,material-ref ,material))
       (gpu:with-shader (shader ,material-ref)
         (setf ,@(loop :for (k v) :on bindings :by #'cddr
                       :collect `(mat-uniform-ref ,material-ref ,k)
                       :collect v))
         (bind-material ,material-ref)
         (with-depth-function ,material-ref
           ,@body)))))

(defun load-materials (core)
  (u:do-hash-values (profile (meta 'material-profiles))
    (%add-material-profile profile core))
  (u:do-hash-values (material-func (meta 'materials))
    (%add-material (funcall material-func core) core))
  (resolve-all-materials core))
