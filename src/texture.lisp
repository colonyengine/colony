(in-package :%fl)

;; The textures-table is a data store of everything in the .tex extension.  This
;; includes any texture-profiles, and defined textures (as texture-descriptors.
;; It is kept in a slot in core-state.
(defclass textures-table ()
  ((%profiles :reader profiles
              :initarg :profiles
              :initform (au:dict #'eq))
   ;; All texture-descriptors from DEFINE-TEXTURE are stored here.
   (%semantic-texture-descriptors :reader semantic-texture-descriptors
                                  :initarg :semantic-texture-descriptors
                                  :initform (au:dict #'eq))
   ;; If a material requires a texture, but it was procedural, we mark a note
   ;; of it in here so the gamedev can find it later and generate them before
   ;; the game starts.
   (%unrealized-procedural-textures :reader unrealized-procedural-textures
                                    :initarg :unrealized-procedural-textures
                                    :initform (au:dict #'equal))))

(defun %make-textures-table (&rest init-args)
  (apply #'make-instance 'textures-table init-args))

;; TODO: Candidate for public API
(defun add-texture-profile (profile core-state)
  (setf (au:href (profiles (textures core-state)) (name profile))
        profile))

;; TODO: Candidate for public API
(defun remove-texture-profile (profile-name core-state)
  (remhash profile-name (profiles (textures core-state))))


;; TODO: Candidate for public API
(defun add-semantic-texture-descriptor (texdesc core-state)
  (setf (au:href (semantic-texture-descriptors (textures core-state))
                 (name texdesc))
        texdesc))

;; TODO: Candidate for public API
(defun add-unrealized-texture (texture context)
  "Add a TEXTURE, which only has defined an opengl texid, to core-state. This
book keeps unrealized textures for the user to finalize before the game starts."
  (setf (au:href (unrealized-procedural-textures
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
  (au:hash-values
   (unrealized-procedural-textures (textures (core-state context)))))

;; TODO: Candidate for public API.
(defun get-procedural-texture-descriptors (context)
  "Return a list of all procedural texture descriptors."
  (let ((results ()))
    (au:do-hash-values (texdesc (semantic-texture-descriptors
                                 (textures (core-state context))))
      (when (eq (texture-type texdesc) :procedural)
        (push texdesc results)))
    (nreverse results)))


;; TODO: Candidate for public API
(defun find-semantic-texture-descriptor (semantic-texture-name context)
  (au:href (semantic-texture-descriptors (textures (core-state context)))
           semantic-texture-name))

;; TODO: Candidate for public API
(defclass texture-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (au:dict #'eq))))

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
                :initform (au:dict #'eq))
   ;; Up to date attributes once the profiles have been applied.
   (%applied-attributes :accessor applied-attributes
                        :initarg :applied-attributes
                        :initform (au:dict #'eq))))

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
    (au:do-hash (key value (attributes texdesc))
      (setf (au:href (attributes new-texdesc) key) (copy-thing value)))

    (au:do-hash (key value (applied-attributes texdesc))
      (setf (au:href (applied-attributes new-texdesc) key) (copy-thing value)))

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
  (au:href (applied-attributes (semantic-texdesc texture)) attribute-name))

(defun (setf get-semantic-applied-attribute) (newval texture attribute-name)
  (setf (au:href (applied-attributes (semantic-texdesc texture))
                 attribute-name)
        newval))

(defun get-computed-applied-attribute (texture attribute-name)
  (au:href (applied-attributes (computed-texdesc texture)) attribute-name))

(defun (setf get-computed-applied-attribute) (newval texture attribute-name)
  (setf (au:href (applied-attributes (computed-texdesc texture))
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
            :do (au:when-found (value (au:href applied-attributes
                                               putative-parameter))
                  (gl:tex-parameter texture-type putative-parameter value))))))

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
  #((:local \"a/b/c/foo.tga\") (:local \"a/b/c/foo.tga\"))

If KIND is :3d, then Data must be an array of slices of mipmap images:
  #(#((:local \"a/b/c/slice0-mip0.tga\") (:local \"a/b/c/slice1-mip0.tga\")))

The same vector structure is returned but with the local descriptor lists
replaced by actual IMAGE instances of the loaded images.
"
  (flet ((read-image-contextually (loc)
           (read-image context loc)))
    (if use-mipmaps-p
        (ecase kind
          ((:1d :2d)
           (map 'vector #'read-image-contextually data))
          (:3d
           (map 'vector (lambda (slices)
                          (map 'vector #'read-image-contextually slices))
                data)))
        (ecase kind
          ((:1d :2d)
           (vector (read-image-contextually (aref data 0))))
          (:3d
           (vector (map 'vector #'read-image-contextually (aref data 0))))))))

(defun free-mipmap-images (images kind)
  "Free all main memory associated with the vector of image objects in IMAGES."
  (loop :for potential-image :across images
        :do
           (ecase kind
             ((:1d :2d)
              (free-storage potential-image))
             (:3d
              (loop :for actual-image :across potential-image
                    :do (free-storage actual-image))))))

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
         (num-mipmaps (length data)))

    (when (and use-mipmaps-p
               (= num-mipmaps 1) ;; We didn't supply any but the base image.
               (> max-mipmaps 1)) ;; And we're expecting some to exist.
      #++(format t "Generating mipmaps!~%")
      (gl:generate-mipmap texture-type))))


(defmethod load-texture-data ((texture-type (eql :texture-1d)) texture context)
  ;; TODO: This assumes no use of the general-data-descriptor or procedurally
  ;; generated content.

  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data)))

    ;; load all of the images we may require.
    (let ((images (read-mipmap-images context data use-mipmaps-p :1d)))

      ;; Check to ensure they all fit into texture memory.
      ;;
      ;; TODO: Refactor out of each method into validate-mipmap-images and
      ;; generalize.
      (loop :for image :across images
            :for location :across data
            :do (when (> (max (height image) (width image))
                         (gl:get-integer :max-texture-size))
                  (error "Image ~A for 1D texture ~A is to big to be loaded onto this card. Max resolution is ~A in either dimension."
                         location
                         (name texture)
                         (gl:get-integer :max-texture-size))))

      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          (compute-mipmap-levels (width (aref images 0))
                                 (height (aref images 0)))

        (validate-mipmap-images images texture
                                expected-mipmaps expected-resolutions)

        (potentially-degrade-texture-min-filter texture)

        ;; Allocate immutable storage if required.
        (when immutable-p
          (let ((num-mipmaps-to-generate
                  (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
            (%gl:tex-storage-1d texture-type num-mipmaps-to-generate
                                (internal-format (aref images 0))
                                (width (aref images 0)))))

        ;; Upload all of the mipmap images into the texture ram.
        ;; TODO: Make this higher order.
        (loop :for idx :below (if use-mipmaps-p (length images) 1)
              :for level = (+ texture-base-level idx)
              :for image = (aref images idx)
              :do (with-slots (%width %height %internal-format %pixel-format
                               %pixel-type %data)
                      image
                    (if immutable-p
                        (gl:tex-sub-image-1d texture-type level 0
                                             %width
                                             %pixel-format %pixel-type %data)
                        (gl:tex-image-1d texture-type level %internal-format
                                         %width 0
                                         %pixel-format %pixel-type %data))))

        ;; And clean up main memory.
        ;; TODO: For procedural textures, this needs evolution.
        (free-mipmap-images images :1d)

        ;; Determine if opengl should generate the mipmaps.
        (potentially-autogenerate-mipmaps texture-type texture)))))

(defmethod load-texture-data ((texture-type (eql :texture-2d)) texture context)
  ;; TODO: This assumes no use of the general-data-descriptor or procedurally
  ;; generated content.

  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data)))

    ;; load all of the images we may require.
    (let ((images (read-mipmap-images context data use-mipmaps-p :2d)))

      ;; Check to ensure they all fit into texture memory.
      ;;
      ;; TODO: Refactor out of each method into validate-mipmap-images and
      ;; generalize.
      (loop :for image :across images
            :for location :across data
            :do (when (> (max (height image) (width image))
                         (gl:get-integer :max-texture-size))
                  (error "Image ~A for texture ~A is to big to be loaded onto this card. Max resolution is ~A in either dimension."
                         location
                         (name texture)
                         (gl:get-integer :max-texture-size))))

      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          (compute-mipmap-levels (width (aref images 0))
                                 (height (aref images 0)))

        (validate-mipmap-images images texture
                                expected-mipmaps expected-resolutions)

        (potentially-degrade-texture-min-filter texture)

        ;; Allocate immutable storage if required.
        (when immutable-p
          (let ((num-mipmaps-to-generate
                  (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
            (%gl:tex-storage-2d texture-type num-mipmaps-to-generate
                                (internal-format (aref images 0))
                                (width (aref images 0))
                                (height (aref images 0)))))

        ;; Upload all of the mipmap images into the texture ram.
        ;; TODO: Make this higher order.
        (loop :for idx :below (if use-mipmaps-p (length images) 1)
              :for level = (+ texture-base-level idx)
              :for image = (aref images idx)
              :do (with-slots (%width %height %internal-format %pixel-format
                               %pixel-type %data)
                      image
                    (if immutable-p
                        (gl:tex-sub-image-2d texture-type level 0 0
                                             %width %height
                                             %pixel-format %pixel-type %data)
                        (gl:tex-image-2d texture-type level %internal-format
                                         %width %height 0
                                         %pixel-format %pixel-type %data))))

        ;; And clean up main memory.
        ;; TODO: For procedural textures, this needs evolution.
        (free-mipmap-images images :2d)

        ;; Determine if opengl should generate the mipmaps.
        (potentially-autogenerate-mipmaps texture-type texture)))))

(defun slices-to-volume (images)
  "Take an array of image objects in IMAGES, all the same height and width and
mipmap level, and assuming index 0 is the 'back slice' and the highest index is
the 'front slice' (according to opengl's 3d texture definition) and then
assemble them into a single linear array suitable for a 3d texture for
opengl. Return a linear array of UNSIGNED-BYTEs that hold the volumentric data."
  (let* ((first-slice (aref images 0))
         (max-depth (length images))
         (pixel-size (get-pixel-size first-slice)))

    (with-slots (%width %height) first-slice ;; all slices the same.
      (let* ((volume-data-size (* %width %height max-depth pixel-size))
             (volume-data (make-array volume-data-size
                                      :element-type '(unsigned-byte 8))))

        #++(format t "slices-to-volume: slices = ~A, volume-data-size = ~A bytes~%"
                   (length images) volume-data-size)

        ;; Recontextualize the 3d images into a real volume array specified as
        ;; a linear array.
        ;;
        ;; NOTE: Assuming row major ordering for all arrays.
        (loop :for d :below max-depth
              :for image = (aref images d) :do
              #++(format t "Processing slice image index ~A: Image ~A~%"
                         d image)
              ;; x and y in terms of images.
                 (loop :for w :below %width :do
                   (loop :for h :below %height :do
                     (let* ((pixel-start-2d
                              (+ (* h (* %width pixel-size))
                                 (* w pixel-size)))
                            (pixel-start-3d
                              (+ (* d (* %width %height pixel-size))
                                 (* h (* %width pixel-size))
                                 (* w pixel-size))))

                       ;; TODO: Crappily copy over the individual pixel data
                       ;; from the 2d image to the 3d image. I should poke this
                       ;; more to see if I can copy way more data at once.
                       (replace volume-data (data image)
                                :start1 pixel-start-3d
                                :end1 (+ pixel-start-3d pixel-size)
                                :start2 pixel-start-2d
                                :end2 (+ pixel-start-2d pixel-size))))))

        #++(format t "Finished constructing volume data.~%")
        volume-data))))

(defmethod load-texture-data ((texture-type (eql :texture-3d)) texture context)
  ;; Determine if loading :images or :volume

  ;; TODO: Validating a 3d texture.
  ;; 0. Ensure that each mipmap level has the same number of slices.
  ;; 1. Ensure that all mipmap levels have the same number of slices.
  ;; 2. Ensure that all required mipmaps levels are present and correct.
  ;; 2. Ensure that each mipmap level fits into the current limits on the card.

  ;; TODO: Fix me. Need to support :layout properly. Currently, this form
  ;; represents opengl defaults.
  (let ((hardcoded-layout '((:origin :left-back-bottom)
                            (:shape (:slices :back-to-front))))
        (current-layout (get-computed-applied-attribute texture :layout)))
    (unless (equal current-layout hardcoded-layout)
      (error "3D Texture ~A has layout:~%  ~S~%but it can only have this as its layout:~%  ~S"
             (name texture) current-layout hardcoded-layout)))

  (let* ((use-mipmaps-p
           (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p
           (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (max-texture-3d-size (gl:get-integer :max-3d-texture-size))
         (data (get-computed-applied-attribute texture :data))
         (num-mipmaps (length data)))

    #++(format t "Attempting to load 3d texture ~A onto GPU: immutable = ~A~%"
               (name texture) immutable-p)

    ;; Load all of our images for each mipmap level, if needed.
    (let* ((all-slices (read-mipmap-images context data use-mipmaps-p :3d))
           (first-image (aref (aref all-slices 0) 0))
           (depth (length (aref all-slices 0))))

      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          (compute-mipmap-levels (width first-image)
                                 (height first-image)
                                 depth)
        #++(format t "expected mipmaps: ~A expected-resolutions: ~A~%"
                   expected-mipmaps expected-resolutions)

        ;; TODO: Fix this call.
        #++(validate-mipmap-images images texture
                                   expected-mipmaps expected-resolutions)
        (potentially-degrade-texture-min-filter texture)

        ;; Allocate immutable storage if required.
        (when immutable-p
          (let ((num-mipmaps-to-generate
                  (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
            (%gl:tex-storage-3d texture-type num-mipmaps-to-generate
                                (internal-format first-image)
                                (width first-image)
                                (height first-image)
                                depth)))

        ;; Load all volumetric mipmaps into the gpu.
        (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
              :for level = (+ texture-base-level idx)
              :for volume = (slices-to-volume (aref all-slices idx))
              :for (mipmap-width mipmap-height mipmap-depth)
                :in expected-resolutions
              :do (with-slots (%width %height %internal-format %pixel-format
                               %pixel-type %data)
                      (aref (aref all-slices idx) 0)
                    #++(format t "Uploading tp GPU 3d mipmap image at level ~A with resolution (w:~A h:~A d:~A)~%"
                               level mipmap-width mipmap-height mipmap-depth)
                    (if immutable-p
                        (gl:tex-sub-image-3d texture-type level 0 0 0
                                             mipmap-width
                                             mipmap-height
                                             mipmap-depth
                                             %pixel-format %pixel-type volume)
                        (gl:tex-image-3d texture-type level %internal-format
                                         mipmap-width
                                         mipmap-height
                                         mipmap-depth 0
                                         %pixel-format %pixel-type volume))))

        (free-mipmap-images all-slices :3d)

        (potentially-autogenerate-mipmaps texture-type texture)

        ))))

(defun lines-to-plane (images)
  "Take an array of image objects in IMAGES, all the same width and mipmap
level, and assuming index 0 is the first layer and the highest index is the last
layer (according to opengl's 1d texture array texture definition) and then
assemble them into a single linear array suitable for a 2d texture for
opengl. Return a linear array of UNSIGNED-BYTEs that hold the planar data."
  (let* ((first-slice (aref images 0))
         (max-height (length images))
         (pixel-size (get-pixel-size first-slice)))

    (with-slots (%width) first-slice ;; all slices the same.
      (let* ((planar-data-size (* %width max-height pixel-size))
             (planar-data (make-array planar-data-size
                                      :element-type '(unsigned-byte 8))))

        (format t "lines-to-plane: numlines = ~A, planar-data-size = ~A bytes~%"
                (length images) planar-data-size)

        ;; Recontextualize the 1d images into a planar array specified as
        ;; a linear array.
        ;;
        ;; NOTE: Assuming row major ordering for all arrays.
        (loop :for h :below max-height
              :for image = (aref images h) :do
                (let* ((row-start-2d
                         ;; always start at start of a 2d row.
                         (+ (* h (* %width pixel-size))
                            (* 0 pixel-size)))
                       ;; Always starting at start of 1d row.
                       (row-start-1d 0))

                  ;; Copy the whole 1d line into the 2d image.
                  (replace planar-data (data image)
                           :start1 row-start-2d
                           :end1 (+ row-start-2d
                                    (* pixel-size %width))
                           :start2 row-start-1d
                           :end2 (+ row-start-1d
                                    (* pixel-size %width)))))

        planar-data))))

(defmethod load-texture-data ((texture-type (eql :texture-1d-array)) texture context)
  (error "load-texture-data: :texture-1d-array implement me")
  nil)

(defmethod load-texture-data ((texture-type (eql :texture-2d-array)) texture context)
  (error "load-texture-data: :texture-2d-array implement me")
  nil)

(defmethod load-texture-data ((texture-type (eql :texture-rectangle)) texture context)
  (error "load-texture-data: :texture-rectangle implement me")
  ;; Determine if loading :image or :planar
  nil)

(defmethod load-texture-data ((texture-type (eql :texture-cube-map)) texture context)
  (error "load-texture-data: :texture-cube-map implement me")
  nil)

(defmethod load-texture-data ((texture-type (eql :texture-cube-map-array)) texture context)
  (error "load-texture-data: :texture-cube-map-array implement me")
  nil)

(defmethod load-texture-data ((texture-type (eql :texture-buffer)) texture context)
  ;; NOTE: this one might be a little harder to get right, since the rcache
  ;; stuff might end up being wrong since this is a buffer object, not a
  ;; traditional texture. So pay attention while implementing this one.
  (error "load-texture-data: :texture-buffer implement me")
  nil)




;; TODO: Candidate for user API.
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
  (let ((texprof (gensym "TEXTURE-PROFILE")))
    `(let* ((,texprof (make-texture-profile :name ',name)))
       (setf ,@(loop :for (attribute value) :in body-form :appending
                     `((au:href (attributes ,texprof) ,attribute) ,value)))
       ,texprof)))

(defmacro define-texture-profile (name &body body)
  "Define a set of attribute defaults that can be applied while defining a texture."
  (let ((texprof (gensym "TEXPROF")))
    `(let* ((,texprof ,(parse-texture-profile name body)))
       (declare (special %temp-texture-profiles))
       (setf (au:href %temp-texture-profiles (name ,texprof)) ,texprof))))

(defmacro define-texture (name (textype &rest profile-overlay-names) &body body)
  "Construct a semantic TEXTURE-DESCRIPTOR and store in the special variable
%TEMP-SEMANTIC-TEXTURE-DESCRIPTORS. NOTE: This us a user facing API macro,
this description, while accurate is utterly not helpful. TODO: Fix."
  (let ((texdesc (gensym "TEXDESC")))
    `(let ((,texdesc (make-texture-descriptor
                      :name ',name
                      :texture-type ',textype
                      :profile-overlay-names ',profile-overlay-names)))
       (declare (special %temp-semantic-texture-descriptors))
       ;; Record the parameters we'll overlay on the profile at use time.
       (setf ,@(loop :for (key value) :in body
                     :append `((au:href (attributes ,texdesc) ,key) ,value))
             ;; NOTE: The form is DEFINE-TEXTURE, but we're actually creating
             ;; semantic-texture-descriptors and storing them. Maybe this naming
             ;; skew should be fix, think about it a little.
             (au:href %temp-semantic-texture-descriptors (name ,texdesc))
             ,texdesc)
       (export ',name))))

(defmethod extension-file-type ((extension-type (eql :textures)))
  "tex")

(defmethod prepare-extension ((extension-type (eql :textures)) core-state)
  (let ((%temp-semantic-texture-descriptors (au:dict #'eq))
        (%temp-texture-profiles (au:dict #'eq)))
    (declare (special %temp-semantic-texture-descriptors %temp-texture-profiles))
    (flet ((%prepare ()
             (map-extensions extension-type (data-path core-state))
             (values %temp-texture-profiles %temp-semantic-texture-descriptors)))
      (multiple-value-bind (profiles texdescs) (%prepare)
        ;; The order doesn't matter. we can type check the texture-descriptors
        ;; after reading _all_ the available textures extensions.
        ;; Process all defined profiles.
        (au:do-hash-values (v profiles)
          (add-texture-profile v core-state))
        ;; Process all texture-descriptors
        (au:do-hash-values (v texdescs)
          (add-semantic-texture-descriptor v core-state))))))

(defun resolve-all-semantic-texture-descriptors (core-state)
  "This is called after all the DEFINE-TEXTURE and DEFINE-TEXTURE-PROFILE forms
are loaded in the extensions.
Ensure that these aspects of texture profiles and desdcriptors are ok:
1. The FL.TEXTURES:DEFAULT-PROFILE exists.
2. Each texture-descriptor has an updated applied-profile set of attributes.
3. All currently known about texture descriptors have valid profile references.
4. All images specified by paths actually exist at that path.
5. The texture type is valid."
  (symbol-macrolet ((profiles (profiles (textures core-state)))
                    (default-profile-name (au:ensure-symbol 'default-profile 'fl.textures)))
    ;; 1. Check for fl.textures:default-profile
    (unless (au:href profiles default-profile-name)
      (error "Default-profile for texture descriptors is not defined."))
    ;; 2. For each texture-descriptor, apply all the profiles in order.
    ;; 3. Check that the specified profiles are valid.
    (au:do-hash-values (v (semantic-texture-descriptors (textures core-state)))
      (let* ((profile-overlays
               ;; First, gather the specified profile-overlays
               (loop :for profile-overlay-name :in (profile-overlay-names v)
                     :collect
                     (au:if-found (concrete-profile (au:href profiles profile-overlay-name))
                                  concrete-profile
                                  (error "Texture profile ~A does not exist."
                                         profile-overlay-name))))
             (profile-overlays
               ;; Then, if we don't see a profile-default in there, we
               ;; put it first automatically.
               (if (member default-profile-name (profile-overlay-names v))
                   profile-overlays
                   (list* (au:href profiles default-profile-name) profile-overlays))))
        ;; Now, overlay them left to right into the applied-attributes table
        ;; in texdesc.
        (dolist (profile-overlay profile-overlays)
          (maphash
           (lambda (key val)
             (setf (au:href (applied-attributes v) key) val))
           (attributes profile-overlay)))
        ;; And finally fold in the texdesc attributes last
        (maphash
         (lambda (key val)
           (setf (au:href (applied-attributes v) key) val))
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

(defun round-down (x)
  (ceiling (- x 1/2)))

(defun round-up (x)
  (floor (+ x 1/2)))

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

(defun compute-mipmap-levels (width height &optional (depth 1))
  "Compute how many mipmaps and what their resolutions must be given a
WIDTH, HEIGHT, and DEPTH (which defaults to 1) size of a texture. We
follow Opengl's formula in dealing with odd sizes (being rounded down).
Return a values of:
  the number of mipmap levels
  the list of resolutions from biggest to smallest each mip map must have."
  (let ((num-levels (+ 1 (floor (log (max width height depth) 2))))
        (resolutions nil))
    (push (list width height depth) resolutions)
    (loop
      :with new-width = width
      :with new-height = height
      :with new-depth = depth
      :for level :below (1- num-levels) :do
        (setf new-width (max (round-down (/ new-width 2)) 1)
              new-height (max (round-down (/ new-height 2)) 1)
              new-depth (max (round-down (/ new-depth 2)) 1))
        (push (list new-width new-height new-depth) resolutions))
    (values num-levels (nreverse resolutions))))
