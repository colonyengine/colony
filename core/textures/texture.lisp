(in-package :%first-light)

(defclass textures-table ()
  ((%profiles :reader profiles
              :initarg :profiles
              :initform (fl.util:dict #'eq))
   ;; All texture-descriptors from DEFINE-TEXTURE are stored here.
   (%semantic-texture-descriptors :reader semantic-texture-descriptors
                                  :initarg :semantic-texture-descriptors
                                  :initform (fl.util:dict #'eq))
   ;; If a material requires a texture, but it was procedural, we mark a note
   ;; of it in here so the gamedev can find it later and generate them before
   ;; the game starts.
   (%unrealized-procedural-textures :reader unrealized-procedural-textures
                                    :initarg :unrealized-procedural-textures
                                    :initform (fl.util:dict #'equal))))

(defun %make-textures-table (&rest init-args)
  (apply #'make-instance 'textures-table init-args))

;; TODO: Candidate for public API
(defun add-texture-profile (profile core-state)
  (setf (fl.util:href (profiles (textures core-state)) (name profile))
        profile))

;; TODO: Candidate for public API
(defun remove-texture-profile (profile-name core-state)
  (remhash profile-name (profiles (textures core-state))))


;; TODO: Candidate for public API
(defun add-semantic-texture-descriptor (texdesc core-state)
  (setf (fl.util:href (semantic-texture-descriptors (textures core-state))
                      (name texdesc))
        texdesc))

;; TODO: Candidate for public API
(defun add-unrealized-texture (texture context)
  "Add a TEXTURE, which only has defined an opengl texid, to core-state. This
book keeps unrealized textures for the user to finalize before the game starts."
  (setf (fl.util:href (unrealized-procedural-textures
                       (textures (core-state context)))
                      (name texture))
        texture))

;; TODO: Candidate for public API
(defun remove-unrealized-texture (texture context)
  "Remove the unrealized TEXTURE from the texture tables. It is assumed that
you have just realized it by setting its opengl parameters and uploading data
or declaring storage of some kind to the GPU."
  (remhash (name texture)
           (unrealized-procedural-textures (textures (core-state context)))))

;; TODO: Candidate for public API
(defun get-unrealized-textures (context)
  "Return a list of textures that are specified in materials, but haven't
been completed (no opengl parameters set for them, or data loaded into GPU).
NOTE: These are already in the RCACHE."
  (fl.util:hash-values
   (unrealized-procedural-textures (textures (core-state context)))))

;; TODO: Candidate for public API.
(defun get-procedural-texture-descriptors (context)
  "Return a list of all procedural texture descriptors."
  (let ((results ()))
    (fl.util:do-hash-values (texdesc (semantic-texture-descriptors
                                      (textures (core-state context))))
      (when (eq (texture-type texdesc) :procedural)
        (push texdesc results)))
    (nreverse results)))


;; TODO: Candidate for public API
(defun find-semantic-texture-descriptor (semantic-texture-name context)
  (fl.util:href (semantic-texture-descriptors (textures (core-state context)))
                semantic-texture-name))

;; TODO: Candidate for public API
(defclass texture-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (fl.util:dict #'eq))))

;; TODO: Candidate for public API
(defun make-texture-profile (&rest init-args)
  (apply #'make-instance 'texture-profile init-args))

;; TODO: Candidate for public API
(defclass texture-descriptor ()
  ((%name :accessor name
          :initarg :name)
   ;; Procedural textures have :procedural as a texture-type!
   (%texture-type :accessor texture-type
                  :initarg :texture-type)
   (%profile-overlay-names :accessor profile-overlay-names
                           :initarg :profile-overlay-names)
   ;; Attribute specified in the define-texture form
   (%attributes :accessor attributes
                :initarg :attributes
                :initform (fl.util:dict #'eq))
   ;; Up to date attributes once the profiles have been applied.
   (%applied-attributes :accessor applied-attributes
                        :initarg :applied-attributes
                        :initform (fl.util:dict #'eq))))

;; TODO candidate for public API
(defun make-texture-descriptor (&rest init-args)
  (apply #'make-instance 'texture-descriptor init-args))

;; TODO: Candidate for public API.
(defun copy-texture-descriptor (texdesc)
  (let ((new-texdesc (make-texture-descriptor)))
    (setf
     ;; These are currently symbols.
     (name new-texdesc) (name texdesc)
     (texture-type new-texdesc) (texture-type texdesc)

     ;; This is a list
     (profile-overlay-names new-texdesc)
     (copy-seq (profile-overlay-names texdesc)))

    ;; Then copy over the attributes, we support SIMPLE values such as: string,
    ;; array, list, vector, and symbol.
    (fl.util:do-hash (key value (attributes texdesc))
      (setf (fl.util:href (attributes new-texdesc) key) (copy-thing value)))

    (fl.util:do-hash (key value (applied-attributes texdesc))
      (setf (fl.util:href (applied-attributes new-texdesc) key) (copy-thing value)))

    new-texdesc))


(defclass texture ()
  (;; The texture descriptor as read from the define-texture DSL form.  This
   ;; records the original values for that texture definition which may include
   ;; that it is procedural or not. This texdesc is a reference to the only copy
   ;; of it as read from the DSL and stored in the texture-table in core-state.
   (%semantic-texdesc :reader semantic-texdesc
                      :initarg :semantic-texdesc)

   ;; The texture descriptor as finally computed from the semantic texture
   ;; descriptor before we load/procedurally create the texture data or storage
   ;; on the GPU. Here, the texture-type must be valid among other aspects of
   ;; the texdesc. In the case of a non procedural texture, this can be computed
   ;; automatically, but in the case of a procedural texture, the user
   ;; ultimately must set the slots to valid things. This texdesc is unique to
   ;; this texture instance. This means, suppose in the case of a procedural
   ;; texture, that each texture instance contains a unique computed copy of
   ;; that the semantic-texdesc that directly describes this version of that
   ;; texture.
   (%computed-texdesc :accessor computed-texdesc
                      :initarg :computed-texdesc)

   ;; The name of the texture might be generated off the texdesc name
   ;; for procedural textures.
   (%name :reader name
          :initarg :name)

   ;; The allocated opengl texture id.
   (%texid :reader texid
           :initarg :texid)))

(defun get-semantic-applied-attribute (texture attribute-name)
  (fl.util:href (applied-attributes (semantic-texdesc texture)) attribute-name))

(defun (setf get-semantic-applied-attribute) (newval texture attribute-name)
  (setf (fl.util:href (applied-attributes (semantic-texdesc texture))
                      attribute-name)
        newval))

(defun get-computed-applied-attribute (texture attribute-name)
  (fl.util:href (applied-attributes (computed-texdesc texture)) attribute-name))

(defun (setf get-computed-applied-attribute) (newval texture attribute-name)
  (setf (fl.util:href (applied-attributes (computed-texdesc texture))
                      attribute-name)
        newval))

;; TODO: Candidate public API
(defun set-opengl-texture-parameters (texture)
  (with-accessors ((texture-type texture-type)
                   (applied-attributes applied-attributes))
      (computed-texdesc texture)
    (let ((texture-parameters
            '(:depth-stencil-texture-mode :texture-base-level
              :texture-border-color :texture-compare-func
              :texture-compare-mode :texture-lod-bias
              :texture-min-filter :texture-mag-filter
              :texture-min-lod :texture-max-lod
              :texture-max-level :texture-swizzle-r
              :texture-swizzle-g :texture-swizzle-b
              :texture-swizzle-a :texture-swizzle-rgba
              :texture-wrap-s :texture-wrap-t :texture-wrap-r)))
      (loop :for putative-parameter :in texture-parameters
            :do (fl.util:when-found (value (fl.util:href applied-attributes putative-parameter))
                  (gl:tex-parameter texture-type
                                    putative-parameter
                                    (typecase value
                                      (flm:vec4 (flm:get-array value))
                                      (t value))))))))

;; TODO: These are cut into individual functions for their context. Maybe later
;; I'll see about condensing them to be more concise.

(defgeneric load-texture-data (texture-type texture context)
  (:documentation "Load actual data described in the TEXTURE's texdesc of
TEXTURE-TYPE into the texture memory."))


(defun read-mipmap-images (context data use-mipmaps-p kind)
  "Read the images described in the mipmap location array DATA into main
memory. If USE-MIPMAPS-P is true, then load all of the mipmaps, otherwise only
load the base image, which is the first one in the array. CONTEXT is the
core-state context slot value. Return a vector of image structure from the
function READ-IMAGE.

If KIND is :1d or :2d, then DATA must be an array of location descriptors like:
  #((:project \"a/b/c/foo.tiff\") (:local \"a/b/c/foo.tiff\"))

If KIND is :1d-array, :2d-array, :3d, then DATA must be an array of
slices of mipmap images:
  #(#((:project \"a/b/c/slice0-mip0.tiff\") (:local \"a/b/c/slice1-mip0.tiff\")))

The same vector structure is returned but with the local descriptor lists
replaced by actual IMAGE instances of the loaded images.
"
  (flet ((read-image-contextually (loc)
           (let ((path (apply #'find-resource context (fl.util:ensure-list loc))))
             (fl.image:read-image path)))
         (process-cube-map-mipmaps (cube-data choice-func)
           ;; Process only one cube map right now... when this works, edit it to
           ;; process many cube maps.
           (unless (equal '(:layout :six) (first cube-data))
             (error "read-mipmap-images: :texture-cub-map, invalid :layout ~S, it must be :six at this time." (first cube-data)))

           ;; canonicalize the face name.
           (map 'vector (lambda (face)
                          (list (canonicalize-cube-map-face-signfier
                                 (first face))
                                (second face)))
                ;; sort the faces for loading in the right order.
                (stable-sort
                 ;; convert vector of path-locations to image instances.
                 (map 'vector
                      (lambda (face)
                        (let ((face-signifier (first face))
                              (mipmaps (second face)))
                          (list face-signifier
                                (funcall choice-func mipmaps))))
                      (second cube-data))
                 #'sort-cube-map-faces-func :key #'first))))

    (if use-mipmaps-p
        ;; Processing contained mipmaps
        (ecase kind
          ((:1d :2d)
           (map 'vector #'read-image-contextually data))
          ((:1d-array :2d-array :3d)
           (map 'vector (lambda (slices)
                          (map 'vector #'read-image-contextually slices))
                data))
          ((:cube-map :cube-map-array)
           (map 'vector
                (lambda (cube-data)
                  (process-cube-map-mipmaps
                   cube-data
                   (lambda (mipmaps) (map 'vector #'read-image-contextually mipmaps))))
                data)))


        ;; Processing only the first image location (mipmap 0)
        (ecase kind
          ((:1d :2d)
           (vector (read-image-contextually (aref data 0))))
          ((:1d-array :2d-array :3d)
           (vector (map 'vector #'read-image-contextually (aref data 0))))
          ((:cube-map :cube-map-array)
           ;; TODO: Test this path.
           (map 'vector
                (lambda (cube-data)
                  (process-cube-map-mipmaps
                   cube-data
                   (lambda (mipmaps) (map 'vector #'read-image-contextually
                                          (vector (aref mipmaps 0))))))
                data))))))

(defun free-mipmap-images (images kind)
  "Free all main memory associated with the vector of image objects in IMAGES."
  (loop :for potential-image :across images
        :do
           (ecase kind
             ((:1d :2d)
              (fl.image:free-storage potential-image))
             ((:1d-array :2d-array :3d)
              (loop :for actual-image :across potential-image
                    :do (fl.image:free-storage actual-image)))
             ((:cube-map :cube-map-array)
              (loop :for face :across potential-image
                    :do (loop :for actual-image :across (second face)
                              :do (fl.image:free-storage actual-image)))))))

(defun validate-mipmap-images (images texture
                               expected-mipmaps expected-resolutions)
  "Given the settings in the TEXTURE, validate the real image objects in the
IMAGES vector to see if we have the expected number and resolution of mipmaps."
  (declare (ignorable expected-resolutions))

  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (num-mipmaps (length images))
         (texture-name (name texture)))

    ;; We need at least one base image.
    ;; TODO: When dealing with procedurally generated textures, this needs
    ;; to be evolved.
    (unless (> num-mipmaps 0)
      (error "Texture ~A specifies no images! Please specify an image!"
             texture-name))

    (when use-mipmaps-p
      (cond
        ;; We have a SINGLE base mipmap (and we're generating them all).
        ((= num-mipmaps 1)
         ;; TODO: Check resolution.
         nil)
        ;; We have the exact number of expected mipmaps.
        ((and (= num-mipmaps expected-mipmaps)
              (<= num-mipmaps max-mipmaps))
         ;; TODO: Check resolutions.
         nil)
        ;; We have the exact number of mipmaps required that fills the
        ;; entire range expected by this texture.
        ((= num-mipmaps max-mipmaps)
         ;; TODO: Check resolutions
         nil)
        ;; Otherwise, something went wrong.
        ;; TODO: Should do a better error message which diagnoses what's wrong.
        (t
         (error "Texture ~A mipmap levels are incorrect:~%~2Ttexture-base-level = ~A~%~2Ttexture-max-level = ~A~%~2Tnumber of mipmaps specified in texture = ~A~%~2Texpected number of mipmaps = ~A~%Probably too many or to few specified mipmap images."
                texture-name
                texture-base-level
                texture-max-level
                num-mipmaps
                expected-mipmaps))))
    ))

(defun potentially-degrade-texture-min-filter (texture)
  "If the TEXTURE is not using mipmaps, fix the :texture-min-filter attribute
on the texture to something appropriate if it is currently set to using
mipmap related interpolation."
  (let ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
        (texture-name (name texture)))
    (symbol-macrolet ((current-tex-min-filter
                        (get-computed-applied-attribute texture
                                                        :texture-min-filter)))

      (unless use-mipmaps-p
        (case current-tex-min-filter
          ((:nearest-mipmap-nearest :nearest-mipmap-linear)
           (warn "Down converting nearest texture min mipmap filter due to disabled mipmaps. Please specify an override :texture-min-filter for texture ~A"
                 texture-name)
           (setf current-tex-min-filter :nearest))
          ((:linear-mipmap-nearest :linear-mipmap-linear)
           (warn "Down converting linear texture min mipmap filter due to disabled mipmaps. Please specify an override :texture-min-filter for texture ~A"
                 texture-name)
           (setf current-tex-min-filter :linear)))))))

(defun potentially-autogenerate-mipmaps (texture-type texture)
  "If, for this TEXTURE, we are using mipmaps, and only supplied a single base
image, then we use GL:GENERASTE-MIPMAP to auto create all of the mipmaps
in the GPU memory."
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data))
         (num-mipmaps
           (ecase texture-type
             ((:texture-1d :texture-2d :texture-3d)
              (length data))
             ((:texture-1d-array :texture-2d-array)
              (length (aref data 0)))
             ((:texture-cube-map :texture-cube-map-array)
              ;; TODO: This assumes no errors in specification of the number of
              ;; mipmaps.
              (let* ((cube (second (aref data 0)))
                     (face (aref cube 0))
                     (mipmaps (second face)))
                (length mipmaps))))))

    (when (and use-mipmaps-p
               (= num-mipmaps 1) ;; We didn't supply any but the base image.
               (> max-mipmaps 1)) ;; And we're expecting some to exist.
      (gl:generate-mipmap texture-type))))


;; todo: Candidate for user API.
(defun procedural-texture-descriptor-p (texdesc)
  (eq (texture-type texdesc) :procedural))

;; TODO: Candidate for user API.
(defun procedural-texture-p (texture)
  (procedural-texture-descriptor-p (semantic-texdesc texture)))

(defun generate-computed-texture-descriptor (texture)
  "Perform an identity copy of the semantic texture descriptor in TEXTURE
and assign it to the computed texture descriptor slot in TEXTURE."
  (setf (computed-texdesc texture)
        (copy-texture-descriptor (semantic-texdesc texture))))

(defun load-texture (context texture-name)
  ;; If we're calling this, it is because we didn't find a texture instance
  ;; with the given texture-name in the rcache.
  (let ((semantic-texdesc
          (find-semantic-texture-descriptor
           (semantic-texture-name texture-name) context)))
    (unless semantic-texdesc
      (error "Cannot load semantic texture-descriptor with unknown name: ~A"
             texture-name))

    (let* ((id (gl:gen-texture))
           ;; TODO: tex-name is wrong here. It shoudl be what the material
           ;; believed it to be or the canonicalname of it if only a symbol.
           (texture (make-instance 'texture
                                   :name texture-name
                                   :semantic-texdesc semantic-texdesc
                                   :texid id)))

      (cond
        ;; Non procedural textures have their semantic texdescs automatically
        ;; converted to computed-texdesc and their information is loaded
        ;; immediately onto the GPU.
        ((not (procedural-texture-p texture))
         (generate-computed-texture-descriptor texture)
         (with-accessors ((computed-texdesc computed-texdesc)) texture
           (gl:bind-texture (texture-type computed-texdesc) id)
           (set-opengl-texture-parameters texture)
           (load-texture-data (texture-type computed-texdesc) texture context)))
        ;; Procedural textures are completely left alone EXCEPT for the texid
        ;; assigned above. It is up to the user to define all the opengl
        ;; parameters and GPU data for this texture.
        (t
         (add-unrealized-texture texture context)
         ;; TODO: Possibly record a note that someone asked for this
         ;; specific texture name, so in the prologue function, the gamedev can
         ;; know the entire set they need to create that is eagerly going to
         ;; be used.
         nil))
      texture)))

(defun parse-texture-profile (name body-form)
  (fl.util:with-unique-names (texprof)
    `(let* ((,texprof (make-texture-profile :name ',name)))
       (setf ,@(loop :for (attribute value) :in body-form :appending
                     `((fl.util:href (attributes ,texprof) ,attribute) ,value)))
       ,texprof)))

(defmacro define-texture-profile (name &body body)
  "Define a set of attribute defaults that can be applied while defining a texture."
  (fl.util:with-unique-names (profiles profile)
    `(symbol-macrolet ((,profiles (fl.data:get 'texture-profiles)))
       (let ((,profile ,(parse-texture-profile name body)))
         (unless ,profiles
           (fl.data:set 'texture-profiles (fl.util:dict #'eq)))
         (setf (fl.util:href ,profiles (name ,profile)) ,profile)))))

(defmacro define-texture (name (textype &rest profile-overlay-names) &body body)
  "Construct a semantic TEXTURE-DESCRIPTOR. "
  (fl.util:with-unique-names (definition desc)
    `(symbol-macrolet ((,definition (fl.data:get 'textures)))
       (let ((,desc (make-texture-descriptor
                     :name ',name
                     :texture-type ',textype
                     :profile-overlay-names ',profile-overlay-names)))
         ;; Record the parameters we'll overlay on the profile at use time.
         (setf ,@(loop :for (key value) :in body
                       :append `((fl.util:href (attributes ,desc) ,key) ,value)))
         (unless ,definition
           (fl.data:set 'textures (fl.util:dict #'eq)))
         (setf (fl.util:href ,definition ',name) ,desc)
         (export ',name)))))

(defun resolve-all-semantic-texture-descriptors (core-state)
  " Ensure that these aspects of texture profiles and desdcriptors are ok:
1. The FL.TEXTURES:DEFAULT-PROFILE exists.
2. Each texture-descriptor has an updated applied-profile set of attributes.
3. All currently known about texture descriptors have valid profile references.
4. All images specified by paths actually exist at that path.
5. The texture type is valid."
  (symbol-macrolet ((profiles (profiles (textures core-state)))
                    (default-profile-name (fl.util:ensure-symbol 'default-profile 'fl.textures)))
    ;; 1. Check for fl.textures:default-profile
    (unless (fl.util:href profiles default-profile-name)
      (error "Default-profile for texture descriptors is not defined."))
    ;; 2. For each texture-descriptor, apply all the profiles in order.
    ;; 3. Check that the specified profiles are valid.
    (fl.util:do-hash-values (v (semantic-texture-descriptors (textures core-state)))
      (let* ((profile-overlays
               ;; First, gather the specified profile-overlays
               (loop :for profile-overlay-name :in (profile-overlay-names v)
                     :collect
                     (fl.util:if-found (concrete-profile (fl.util:href profiles profile-overlay-name))
                                       concrete-profile
                                       (error "Texture profile ~A does not exist."
                                              profile-overlay-name))))
             (profile-overlays
               ;; Then, if we don't see a profile-default in there, we
               ;; put it first automatically.
               (if (member default-profile-name (profile-overlay-names v))
                   profile-overlays
                   (list* (fl.util:href profiles default-profile-name) profile-overlays))))
        ;; Now, overlay them left to right into the applied-attributes table
        ;; in texdesc.
        (dolist (profile-overlay profile-overlays)
          (maphash
           (lambda (key val)
             (setf (fl.util:href (applied-attributes v) key) val))
           (attributes profile-overlay)))
        ;; And finally fold in the texdesc attributes last
        (maphash
         (lambda (key val)
           (setf (fl.util:href (applied-attributes v) key) val))
         (attributes v)))
      (semantic-texture-descriptors (textures core-state)))

    ;; TODO: 4
    ;; TODO: 5

    nil))

;; public API
(defun general-data-format-descriptor (&key width height depth internal-format pixel-format
                                         pixel-type data)
  "Produce a descriptor for generalized volumetric data to be loaded into a :texture-3d type
texture. If :data has the value :empty, allocate the memory of the size and types specified on the
GPU."
  ;; TODO: Implement me!
  (declare (ignore width height depth internal-format pixel-format pixel-type data))
  #())

;; Interim use of the RCACHE API.

;; TODO: candidate for public API.
(defmethod rcache-layout ((entry-type (eql :texture)))
  '(equalp))

;; TODO: candidate for public API.
(defmethod rcache-construct ((entry-type (eql :texture)) (core-state core-state) &rest keys)
  (destructuring-bind (texture-location) keys
    ;;(format t "Creating texture instance whose name is: ~A~%" texture-location)
    (load-texture (context core-state) texture-location)))

;; TODO: candidate for public API.
(defmethod rcache-dispose ((entry-type (eql :texture)) (core-state core-state) texture)
  (gl:delete-texture (texid texture)))


;; Utility functions for texturing, maybe move elsewhere.

(defun canonicalize-texture-name (texture-name)
  "If the TEXTURE-NAME is a symbol, we canoniclalize it to the list:
 (TEXTURE-NAME 0). If it is a consp, we return it unmodified."
  (cond
    ((symbolp texture-name)
     (list texture-name 0))
    ((consp texture-name)
     texture-name)
    (t
     (error "Unable to canonicalize the texture name: ~A" texture-name))))

;; TODO: Maybe reifiy the semantic-name, texture-instance-name specification
;; into a CLOS object.
(defun semantic-texture-name (texture-name)
  "Given a TEXTURE-NAME (which could be either a symbol as the semantic name of
a texture, or a list when it is an instance of a texture), return just the
semantic name of it which was specified with a DEFINE-TEXTURE."
  (if (consp texture-name)
      (first texture-name)
      texture-name))

(defun load-texture-descriptors (core-state)
  (fl.util:do-hash-values (profile (fl.data:get 'texture-profiles))
    (add-texture-profile profile core-state))
  (fl.util:do-hash-values (desc (fl.data:get 'textures))
    (add-semantic-texture-descriptor desc core-state))
  (resolve-all-semantic-texture-descriptors core-state))
