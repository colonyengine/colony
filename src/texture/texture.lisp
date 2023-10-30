(in-package #:virality.texture)

;; -------------------------------------------------------------------------
;; NOTE: The candidate API functions in here are in the wrong spot, named
;; wrongly, and might not even have to exist. :) They are here because I'm
;; doing layering work to break up tangled up code. No one calls these.

;; TODO: Candidate for public Core API
(defun core/add-semantic-texture-descriptor (texdesc core)
  (textab:add-semantic-texture-descriptor texdesc (v::textures core)))

;; TODO: Candidate for public Core API
(defun core/remove-semantic-texture-descriptor (texdesc core)
  (textab:remove-semantic-texture-descriptor texdesc (v::textures core)))

;; TODO: Candidate for public Context API
(defun context/add-unrealized-texture (texture context)
  "Add a TEXTURE, which only has defined an opengl texid, to core. This book
keeps unrealized textures for the user to finalize before the game starts."
  (textab::add-unrealized-texture texture (v::textures (v::core context))))

;; TODO: Candidate for public Context API
(defun context/remove-unrealized-texture (texture context)
  "Remove the unrealized TEXTURE from the texture tables. It is assumed that
you have just realized it by setting its opengl parameters and uploading data
or declaring storage of some kind to the GPU."
  (textab:remove-unrealized-texture texture (v::textures (v::core context))))

;; TODO: Candidate for public Context API
(defun context/get-unrealized-textures (context)
  "Return a list of textures that are specified in materials, but haven't been
completed (no opengl parameters set for them, or data loaded into GPU).
NOTE: These are already in the resource-cache."
  (textab:get-unrealized-textures (v::textures (v::core context))))

;; TODO: Candidate for public Context API.
(defun context/get-procedural-texture-descriptors (context)
  "Return a list of all procedural texture descriptors."
  (textab:get-procedural-texture-descriptors (v::textures (v::core context))))

;; TODO: Candidate for public Context API
(defun context/find-semantic-texture-descriptor (semantic-texture-name context)
  (textab:find-semantic-texture-descriptor semantic-texture-name
                                           (v::textures (v::core context))))
;; -------------------------------------------------------------------------


;; Implementation of TEXTURE-DESCRIPTOR

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
    (u:do-hash (key value (attributes texdesc))
      (setf (u:href (attributes new-texdesc) key) (u:copy-sequence-tree value)))
    (u:do-hash (key value (applied-attributes texdesc))
      (setf (u:href (applied-attributes new-texdesc) key) (u:copy-sequence-tree value)))
    new-texdesc))



;; Implementation of TEXTURE-PROFILE

;; TODO: Candidate for public API
(defun make-texture-profile (&rest init-args)
  (apply #'make-instance 'texture-profile init-args))

(defun parse-texture-profile (name body-form)
  (u:with-gensyms (texprof)
    `(let* ((,texprof (make-texture-profile :name ',name)))
       (setf ,@(loop :for (attribute value) :in body-form
                     :appending `((u:href (attributes ,texprof) ,attribute)
                                  ,value)))
       ,texprof)))

(defmacro define-texture-profile (name &body body)
  "Define a set of attribute defaults that can be applied while defining a
texture."
  (u:with-gensyms (profile)
    `(let ((,profile ,(parse-texture-profile name body)))
       (setf (u:href v::=meta/texture-profiles= (name ,profile)) ,profile))))



;; Implementation of TEXTURE


(u:define-printer (texture stream :type t)
  (format stream "id: ~s" (texid texture)))

(defun get-semantic-applied-attribute (texture attribute-name)
  (u:href (applied-attributes (semantic-texdesc texture)) attribute-name))

(defun (setf get-semantic-applied-attribute) (new-value texture attribute-name)
  (setf (u:href (applied-attributes (semantic-texdesc texture)) attribute-name)
        new-value))

(defun get-computed-applied-attribute (texture attribute-name)
  (u:href (applied-attributes (computed-texdesc texture)) attribute-name))

(defun (setf get-computed-applied-attribute) (new-value texture attribute-name)
  (setf (u:href (applied-attributes (computed-texdesc texture)) attribute-name)
        new-value))

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
      (dolist (putative-parameter texture-parameters)
        (u:when-found (value (u:href applied-attributes putative-parameter))
          (gl:tex-parameter texture-type
                            putative-parameter
                            value))))))

;;; TODO: These are cut into individual functions for their context. Maybe
;;; later I'll see about condensing them to be more concise.

(defgeneric load-texture-data (texture-type texture context)
  (:documentation "Load actual data described in the TEXTURE's texdesc of ~
TEXTURE-TYPE into the texture memory."))

(defun resolve-image-path (context type asset)
  "Given a CONTEXT and a TYPE of asset-cache, resolve the ASSET into a full
path into the filesystem to that asset. Return this path."
  (v:with-asset-cache context type asset
    (v::resolve-path asset)))

;; TODO: candidate for method.
(defun assert-valid-cube-map-layout (cube-data)
  ;; TODO: Make this check that the :opengl layout has the images in exactly
  ;; the right ordering too.
  (let ((valid-layouts '((:layout :six) (:layout :opengl))))
    (unless (member (first cube-data) valid-layouts :test 'equal)
      (error "assert-valid-cube-map-data: ~
                     :texture-cub-map, invalid :layout ~s, ~
                     it must be one of ~A at this time."
             (first cube-data) valid-layouts))))

;; TODO: candidate for method.
(defun map-cube-layout (func cube-layout)
  "Map FUNC across the items for a single CUBE-LAYOUT no matter what kind of
valid cube layout it is. Return the same layout with the result of the FUNC for
each item."
  (unless (listp cube-layout)
    (error "map-cube-layout: Unknown cube map layout: ~S!" cube-layout))

  (let ((layout (first cube-layout)))
    (ecase (second layout)
      ((:six :opengl)
       (list layout
             (map 'vector
                  (lambda (item)
                    (destructuring-bind (face mipmaps) item
                      (list face (map 'vector func mipmaps))))
                  (second cube-layout)))))))

(defun map-cube-map-data (func cube-data)
  "No matter the kind of layout, map FUNC over the items in the CUBE-DATA and
return the mapped items in the same layout."
  (map 'vector (lambda (l) (map-cube-layout func l)) cube-data))

(defun test-map-cube-layout ()
  (values
   (map-cube-layout
    ;; Simple conversion of item to a system string.
    (lambda (x) (format nil "~(~S~)" x))
    '((:layout :six)
      #((:+X
         #((environments helipad-diffuse-right)))
        (:-X
         #((environments helipad-diffuse-left)))
        (:+Y
         #((environments helipad-diffuse-top)))
        (:-Y
         #((environments helipad-diffuse-bottom)))
        (:+Z
         #((environments helipad-diffuse-front)))
        (:-Z
         #((environments helipad-diffuse-back))))))

   (map-cube-layout
    ;; Simple conversion of item to a system string.
    (lambda (x) (format nil "~(~S~)" x))
    ;; For this layout, the elements must be in the exact order specified.
    '((:layout :opengl)
      #((:texture-cube-map-positive-x
         #((environments helipad-diffuse-right)))
        (:texture-cube-map-negative-x
         #((environments helipad-diffuse-left)))
        (:texture-cube-map-positive-y
         #((environments helipad-diffuse-top)))
        (:texture-cube-map-negative-y
         #((environments helipad-diffuse-bottom)))
        (:texture-cube-map-positive-z
         #((environments helipad-diffuse-front)))
        (:texture-cube-map-negative-z
         #((environments helipad-diffuse-back))))))))

(defun convert-cube-map-layout-to-opengl (cube-data)
  (let ((layout (first cube-data)))
    (cond
      ((equal '(:layout :opengl) layout)
       ;; It already is, so just return it.
       cube-data)
      ((equal '(:layout :six) layout)
       (list '(:layout :opengl)
             (map 'vector
                  (lambda (x)
                    (list (canonicalize-cube-map-face-signfier (first x))
                          (second x)))
                  ;; sort the faces for loading in the right order.
                  (stable-sort
                   (second cube-data)
                   #'sort-cube-map-faces-func :key #'first))))
      (t
       (error "convert-cube-map-layout-to-opengl: Imaplement me for layout: ~A"
              layout)))))

(defun test-convert-cube-map-layout-to-opengl ()
  (convert-cube-map-layout-to-opengl
   '((:layout :six)
     #((:+X
        #((environments helipad-diffuse-right)))
       (:+Y
        #((environments helipad-diffuse-top)))
       (:+Z
        #((environments helipad-diffuse-front)))
       (:-X
        #((environments helipad-diffuse-left)))
       (:-Y
        #((environments helipad-diffuse-bottom)))
       (:-Z
        #((environments helipad-diffuse-back)))))))

;; TODO: Strong candidate for a method
(defun resolve-mipmap-images (context data use-mipmaps-p kind)
  "Return a new form in the same format as DATA but with the asset forms
replaced by fully qualified paths to their location (on disk, etc).
If USE-MIPMAPS-P is true, then resolve all images, otherwise only resolve
and return the first logical image out of the mipmap sequence."

  (flet ((resolve-image-path-contextually (asset)
           (resolve-image-path context :texture asset)))
    (if use-mipmaps-p
        ;; Processing contained mipmaps
        (ecase kind
          ((:1d :2d)
           (map 'vector #'resolve-image-path-contextually data))
          ((:1d-array :2d-array :3d)
           (map 'vector
                (lambda (x)
                  (map 'vector #'resolve-image-path-contextually x))
                data))
          ((:cube-map :cube-map-array)
           (map-cube-map-data #'resolve-image-path-contextually data)))

        ;; Processing only the first image location (mipmap 0)
        (ecase kind
          ((:1d :2d)
           (vector (resolve-image-path-contextually (aref data 0))))
          ((:1d-array :2d-array :3d)
           (vector (map 'vector #'resolve-image-path-contextually
                        (aref data 0))))
          ((:cube-map :cube-map-array)
           (map-cube-map-data #'resolve-image-path-contextually
                              (vector (aref data 0))))))))

;; TODO: Strong candidate for a method
(defun read-mipmap-images (context data use-mipmaps-p kind flip-y)
  "Read the images described in the mipmap location array DATA into main memory.
If USE-MIPMAPS-P is true, then load all of the mipmaps, otherwise only load the
base image, which is the first one in the array. CONTEXT is the core context
slot value. Return a vector of image structure from the function
IMG:LOAD-IMAGE.

If KIND is :1d or :2d, then DATA must be an array of location descriptors like:
#((:project \"a/b/c/foo.png\") (:local \"a/b/c/foo.png\") ...)

If KIND is :1d-array, :2d-array, :3d, then DATA must be an array of slices of
mipmap images:
 #(#((:project \"a/b/c/slice0-mip0.png\") (:local\"a/b/c/slice1-mip0.png\")))

The same vector structure is returned but with the local descriptor lists
replaced by actual IMAGE instances of the loaded images."

  ;; NOTE: resolve-image-path-contextually will restrict the number of images
  ;; for mipmaps depending on value of use-mipmaps-p.
  (let ((data (resolve-mipmap-images context data use-mipmaps-p kind)))
    (flet ((read-image-contextually (asset-path)
             (img:load-image asset-path :flip-y flip-y)))
      ;; Processing contained mipmaps
      (ecase kind
        ((:1d :2d)
         (map 'vector #'read-image-contextually data))
        ((:1d-array :2d-array :3d)
         (map 'vector
              (lambda (x)
                (map 'vector #'read-image-contextually x))
              data))
        ((:cube-map :cube-map-array)
         (map 'vector #'convert-cube-map-layout-to-opengl
              (map-cube-map-data #'read-image-contextually data)))))))

(defun validate-mipmap-images (images texture expected-mipmaps
                               expected-resolutions)
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
    ;; TODO: When dealing with procedurally generated textures, this needs to be
    ;; evolved.
    (unless (plusp num-mipmaps)
      (error "Texture ~a specifies no images! Please specify an image!"
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
        ;; We have the exact number of mipmaps required that fills the entire
        ;; range expected by this texture.
        ((= num-mipmaps max-mipmaps)
         ;; TODO: Check resolutions
         nil)
        ;; Otherwise, something went wrong.
        ;; TODO: Should do a better error message which diagnoses what's wrong.
        (t
         (error "Texture ~a mipmap levels are incorrect:~% ~
                 ~2Ttexture-base-level = ~a~%~2Ttexture-max-level = ~a~% ~
                 ~2Tnumber of mipmaps specified in texture = ~a~% ~
                 ~2Texpected number of mipmaps = ~a~%Probably too many or to ~
                 few specified mipmap images."
                texture-name
                texture-base-level
                texture-max-level
                num-mipmaps
                expected-mipmaps))))))

(defun potentially-degrade-texture-min-filter (texture)
  "If the TEXTURE is not using mipmaps, fix the :texture-min-filter attribute on
the texture to something appropriate if it is currently set to using mipmap
related interpolation."
  (let ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
        (texture-name (name texture)))
    (symbol-macrolet ((current-tex-min-filter
                        (get-computed-applied-attribute
                         texture
                         :texture-min-filter)))
      (unless use-mipmaps-p
        (ecase current-tex-min-filter
          ((:nearest-mipmap-nearest :nearest-mipmap-linear)
           (warn "Down converting nearest texture min mipmap filter due to ~
                  disabled mipmaps. Please specify an override ~
                  :texture-min-filter for texture ~a"
                 texture-name)
           (setf current-tex-min-filter :nearest))
          ((:linear-mipmap-nearest :linear-mipmap-linear)
           (warn "Down converting linear texture min mipmap filter due to ~
                  disabled mipmaps. Please specify an override ~
                  :texture-min-filter for texture ~a"
                 texture-name)
           (setf current-tex-min-filter :linear)))))))

(defun potentially-autogenerate-mipmaps (texture-type texture)
  "If, for this TEXTURE, we are using mipmaps, and only supplied a single base
image, then we use GL:GENERASTE-MIPMAP to auto create all of the mipmaps in the
GPU memory."
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
               (= num-mipmaps 1)  ; We didn't supply any but the base image.
               (> max-mipmaps 1)) ; And we're expecting some to exist.
      (gl:generate-mipmap texture-type))))

;; TODO: Candidate for user API.
(defun procedural-texture-descriptor-p (texdesc)
  (eq (texture-type texdesc) :procedural))

;; TODO: Candidate for user API.
(defun procedural-texture-p (texture)
  (procedural-texture-descriptor-p (semantic-texdesc texture)))

(defun generate-computed-texture-descriptor (texture)
  "Perform an identity copy of the semantic texture descriptor in TEXTURE and
assign it to the computed texture descriptor slot in TEXTURE."
  (setf (computed-texdesc texture)
        (copy-texture-descriptor (semantic-texdesc texture))))

;; TODO 1. make missing texture and missing material not recompiliable.

(defun find-debug-texture (core)
  (let* ((texture-name (canonicalize-texture-name 'x:debug-texture))
         (texture (v::resource-cache-peek
                   (v::context core) :texture texture-name)))
    (unless texture
      (error "FIND-DEBUG-TEXTURE: It is impossible that the debug-texture ~
              doesn't not exist. Fixme."))
    texture))

(defun unload-texture (context texture)
  (gl:delete-texture (texid texture))
  (let ((debug-texture (find-debug-texture (v::core context))))
    (setf (texid texture) (texid debug-texture))))

(defun refresh-texture (context texture texture-name)
  ;; Unload the current texid and shove back into a new texid whatever the
  ;; new texture descriptor for the texture-name indicates.
  (with-accessors ((texid texid)) texture
    (let ((texture-table (v::textures (v::core context))))
      (unload-texture context texture)
      (let((semantic-texdesc
             (textab:find-semantic-texture-descriptor
              (semantic-texture-name texture-name) texture-table))
           (id (gl:gen-texture)))
        (setf (semantic-texdesc texture) semantic-texdesc
              texid id)
        (upload-texture context texture)))))

(defun load-texture (context texture-name)
  "Return a TEXTURE instance that represents the loading of the images required
for TEXTURE-NAME from disk (or elsewhere) and the uploading of it to the GPU.
If TEXTURE-NAME does not resolve to a known semantic texture descriptor then
return the TEXTURE instance for the debug-texture."
  (let ((texture-table (v::textures (v::core context))))
    (u:if-let ((semantic-texdesc
                (textab:find-semantic-texture-descriptor
                 (semantic-texture-name texture-name) texture-table)))
      (let* ((id (gl:gen-texture))
             ;; TODO: tex-name is wrong here. It shoudl be what the material
             ;; believed it to be or the canonicalname of it if only a symbol.
             (texture (make-instance 'texture
                                     :name texture-name
                                     :semantic-texdesc semantic-texdesc
                                     :texid id)))
        (values (upload-texture context texture) t))

      (values (v::resource-cache-lookup
               context :texture
               (canonicalize-texture-name 'x:debug-texture))
              nil))))

(defun upload-texture (context texture)
  (cond
    ;; Non procedural textures have their semantic texdescs automatically
    ;; converted to computed-texdesc and their information is loaded
    ;; immediately onto the GPU.
    ((not (procedural-texture-p texture))
     (generate-computed-texture-descriptor texture)
     (with-accessors ((computed-texdesc computed-texdesc)) texture
       (gl:bind-texture (texture-type computed-texdesc) (texid texture))
       (set-opengl-texture-parameters texture)
       (load-texture-data (texture-type computed-texdesc) texture context)))
    ;; Procedural textures are completely left alone EXCEPT for the texid
    ;; assigned above. It is up to the user to define all the opengl
    ;; parameters and GPU data for this texture.
    (t
     (textab:add-unrealized-texture texture (v::textures (v::core context)))
     ;; TODO: Possibly record a note that someone asked for this specific
     ;; texture name, so in the prologue function, the gamedev can know the
     ;; entire set they need to create that is eagerly going to be used.
     nil))
  texture)


(defun update-texture (context old-descriptor new-descriptor)
  (tpool:push-queue
   (v::thread-pool v::*core-debug*)
   :recompile
   (list
    :texture
    (lambda (core)
      (with-accessors ((texture-table v::textures)) core
        (if old-descriptor
            (let* ((old-name (canonicalize-texture-name (name old-descriptor)))
                   (old-texture (v::resource-cache-peek
                                 (v::context core)
                                 :texture old-name)))
              (textab:remove-semantic-texture-descriptor old-descriptor
                                                         texture-table)
              ;; NOTE: Can't refactor the two lines below cause it needs the
              ;; remove-semantic-texture-descriptor to have been called.
              (textab:add-semantic-texture-descriptor new-descriptor
                                                      texture-table)
              (resolve-semantic-texture-descriptor core new-descriptor)
              (refresh-texture context old-texture old-name))
            (let ((new-name (canonicalize-texture-name (name new-descriptor))))
              (textab:add-semantic-texture-descriptor new-descriptor
                                                      texture-table)
              (resolve-semantic-texture-descriptor core new-descriptor)
              (v::resource-cache-lookup context :texture new-name))))))))

(defun update-texture/interactively (old-descriptor new-descriptor)
  (when (v:core-bound-p v::*core-debug*)
    (let ((context (v:context v::*core-debug*)))
      (update-texture context old-descriptor new-descriptor))))

(defmacro define-texture (name (textype &rest profile-overlay-names) &body body)
  "Construct a semantic TEXTURE-DESCRIPTOR. "
  (u:with-gensyms (desc-lookup old-desc new-desc)
    `(symbol-macrolet ((,desc-lookup (u:href v::=meta/textures= ',name)))
       (let ((,new-desc (make-texture-descriptor
                         :name ',name
                         :texture-type ',textype
                         :profile-overlay-names ',profile-overlay-names))
             (,old-desc ,desc-lookup))
         ;; Record the parameters we'll overlay on the profile at use time.
         (setf ,@(loop :for (key value) :in body
                       :append `((u:href (attributes ,new-desc) ,key)
                                 ,value)))
         (setf ,desc-lookup ,new-desc)
         (update-texture/interactively ,old-desc ,new-desc)))))

(defun resolve-semantic-texture-descriptor (core texture-descriptor)
  (symbol-macrolet ((profiles (textab:profiles (v::textures core)))
                    (default-profile-name 'x:default-profile))
    ;; 1. Check for x:default-profile
    (unless (u:href profiles default-profile-name)
      (error "Default-profile for texture descriptors is not defined."))
    ;; 2. For each texture-descriptor, apply all the profiles in order.
    ;; 3. Check that the specified profiles are valid.
    ;; TODO: 4
    ;; TODO: 5
    (let* ((profile-overlays
             ;; First, gather the specified profile-overlays
             (loop :for profile-overlay-name :in (profile-overlay-names
                                                  texture-descriptor)
                   :collect
                   (u:if-found (concrete-profile
                                (u:href profiles profile-overlay-name))
                     concrete-profile
                     (error "Texture profile ~a does not exist."
                            profile-overlay-name))))
           (attributes (attributes texture-descriptor))
           (applied-attributes (applied-attributes texture-descriptor))
           (profile-overlays
             ;; Then, if we don't see a profile-default in there, we put it
             ;; first automatically.
             (if (member default-profile-name (profile-overlay-names
                                               texture-descriptor))
                 profile-overlays
                 (list* (u:href profiles default-profile-name)
                        profile-overlays))))
      ;; Now, overlay them left to right into the applied-attributes table in
      ;; texdesc.
      (dolist (profile-overlay profile-overlays)
        (maphash
         (lambda (key value)
           (setf (u:href applied-attributes key)
                 value))
         (attributes profile-overlay)))
      ;; And finally fold in the texdesc attributes last
      (maphash
       (lambda (key value)
         (setf (u:href applied-attributes key) value))
       attributes))))

(defun resolve-all-semantic-texture-descriptors (core)
  " Ensure that these aspects of texture profiles and desdcriptors are ok:
1. The X:DEFAULT-PROFILE exists.
2. Each texture-descriptor has an updated applied-profile set of attributes.
3. All currently known about texture descriptors have valid profile references.
4. All images specified by paths actually exist at that path.
5. The texture type is valid."
  (u:do-hash-values (v (textab:semantic-texture-descriptors
                        (v::textures core)))
    (resolve-semantic-texture-descriptor core v)))

;; public API
(defun general-data-format-descriptor (&key width height depth internal-format
                                         pixel-format pixel-type data)
  "Produce a descriptor for generalized volumetric data to be loaded into a
:texture-3d type texture. If :data has the value :empty, allocate the memory of
the size and types specified on the GPU."
  ;; TODO: Implement me!
  (declare (ignore width height depth internal-format pixel-format pixel-type
                   data))
  #())

;;; Interim use of the resource-cache API.

;; TODO: candidate for public API.
(defmethod v::resource-cache-layout ((entry-type (eql :texture)))
  '(equalp))

;; TODO: candidate for public API.
(defmethod v::resource-cache-construct (context (entry-type (eql :texture))
                                        &rest keys)
  (destructuring-bind (texture-location) keys
    (load-texture context texture-location)))

;; TODO: candidate for public API.
(defmethod v::resource-cache-dispose (context (entry-type (eql :texture))
                                      texture)
  (gl:delete-texture (texid texture)))

;;; Utility functions for texturing, maybe move elsewhere.

(defun canonicalize-texture-name (texture-name)
  "If the TEXTURE-NAME is a symbol, we canoniclalize it to the list:
 (TEXTURE-NAME 0). If it is a consp, we return it unmodified."
  (typecase texture-name
    (symbol (list texture-name 0))
    (cons texture-name)
    (t (error "Unable to canonicalize the texture name: ~a" texture-name))))

;; TODO: Maybe reifiy the semantic-name, texture-instance-name specification
;; into a CLOS object.
(defun semantic-texture-name (texture-name)
  "Given a TEXTURE-NAME (which could be either a symbol as the semantic name of
a texture, or a list when it is an instance of a texture), return just the
semantic name of it which was specified with a DEFINE-TEXTURE."
  (if (consp texture-name)
      (first texture-name)
      texture-name))

(defun load-texture-descriptors (core)
  (with-accessors ((texture-table v::textures)) core
    (gl:pixel-store :unpack-alignment 1)
    (u:do-hash-values (profile v::=meta/texture-profiles=)
      (textab:add-texture-profile profile texture-table))
    (u:do-hash-values (desc v::=meta/textures=)
      (textab:add-semantic-texture-descriptor desc texture-table))
    (resolve-all-semantic-texture-descriptors core)))
