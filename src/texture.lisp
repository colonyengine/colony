(in-package :fl.core)

;; The textures-table is a data store of everything in the .tex extension.  This
;; includes any texture-profiles, and defined textures (as texture-descriptors.
;; It is kept in a slot in core-state.
(defclass textures-table ()
  ((%profiles :reader profiles
              :initarg :profiles
              :initform (au:dict #'eq))
   (%texture-descriptors :reader texture-descriptors
                         :initarg :texture-descriptors
                         :initform (au:dict #'eq))))


(defun %make-textures-table (&rest init-args)
  (apply #'make-instance 'textures-table init-args))

(defun %add-texture-profile (profile core-state)
  (setf (au:href (profiles (textures core-state)) (name profile))
        profile))

(defun %add-texture-descriptor (texdesc core-state)
  (setf (au:href (texture-descriptors (textures core-state)) (name texdesc))
        texdesc))


(defclass texture-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (au:dict #'eq))))

;; TODO candidate for public API
(defun make-texture-profile (&rest init-args)
  (apply #'make-instance 'texture-profile init-args))


(defclass texture-descriptor ()
  ((%name :reader name
          :initarg :name)
   (%texture-type :reader texture-type
                  :initarg :texture-type)
   (%profile-overlay-names :reader profile-overlay-names
                           :initarg :profile-overlay-names)
   ;; Attribute specified in the define-texture form
   (%attributes :reader attributes
                :initarg :attributes
                :initform (au:dict #'eq))
   ;; Up to date attributes once the profiles have been applied.
   (%applied-attributes :reader applied-attributes
                        :initarg :applied-attributes
                        :initform (au:dict #'eq))))

;; TODO candidate for public API
(defun make-texture-descriptor (&rest init-args)
  (apply #'make-instance 'texture-descriptor init-args))



(defclass texture ()
  (;; The descriptor from when we derived this texture.
   (%descriptor :reader descriptor
                :initarg :descriptor)

   ;; The allocated opengl texture id.
   (%texid :reader texid
           :initarg :texid)))

;; TODO: Convert to taking a texture-decriptor instead of a location.
(defun load-texture (context location &key
                                        (filter-min :linear-mipmap-linear)
                                        (filter-mag :linear)
                                        (wrap :repeat)
                                        (wrap-s wrap)
                                        (wrap-t wrap))

  (let ((image (read-image context location)))
    (with-slots (%width %height %internal-format %pixel-format
                 %pixel-type %data)
        image

      (let ((id (gl:gen-texture)))
        (gl:bind-texture :texture-2d id)
        (gl:tex-image-2d :texture-2d 0
                         %internal-format %width %height 0 %pixel-format
                         %pixel-type %data)
        (gl:generate-mipmap :texture-2d)
        (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
        (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
        (gl:tex-parameter :texture-2d :texture-min-filter filter-min)
        (gl:tex-parameter :texture-2d :texture-mag-filter filter-mag)
        (free-storage image)

        ;; Wrap it all into a real texture instance for better bookeeping.
        (make-instance 'texture :texid id)))))

(defun set-opengl-texture-parameters (texdesc)
  (with-accessors ((texture-type texture-type)
                   (applied-attributes applied-attributes))
      texdesc
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

(defun upload-texture-images-to-gpu (texdesc context)

  nil)

(defun load-texture-new (context texture-name)
  (let ((texdesc (au:href (texture-descriptors (textures (core-state context)))
                          texture-name)))
    (unless texdesc
      (error "Cannot load texture with unknown name: ~A" texture-name))

    (let ((id (gl:gen-texture)))
      (gl:bind-texture (texture-type texdesc) id)

      (upload-texture-images-to-gpu texdesc context)

      (set-opengl-texture-parameters texdesc)

      (make-instance 'texture :texdesc texdesc :texid id))))



(defun parse-texture-profile (name body-form)
  (let ((texprof (gensym "TEXTURE-PROFILE")))
    `(let* ((,texprof (make-texture-profile :name ',name)))
       (setf
        ,@(loop :for (attribute value) :in body-form :appending
                `((au:href (attributes ,texprof) ,attribute) ,value)))
       ,texprof)))


(defmacro define-texture-profile (name &body body)
  "Define a set of attribute defaults that can be applied while defining
a texture."
  (let ((texprof (gensym "TEXPROF")))
    `(let* ((,texprof ,(parse-texture-profile name body)))
       (declare (special %temp-texture-profiles))
       (setf (au:href %temp-texture-profiles (name ,texprof)) ,texprof))))



(defmacro define-texture (name (textype &rest profile-overlay-names) &body body)
  (let ((texdesc (gensym "TEXDESC")))
    `(let ((,texdesc (make-texture-descriptor
                      :name ',name
                      :texture-type ',textype
                      :profile-overlay-names ',profile-overlay-names)))
       (declare (special %temp-texture-descriptors))
       ;; Record the parameters we'll overlay on the profile at use time.
       (setf
        ,@(loop :for (key value) :in body :appending
                `((au:href (attributes ,texdesc) ,key) ,value)))
       (setf (au:href %temp-texture-descriptors (name ,texdesc)) ,texdesc))))


(defmethod extension-file-type ((extension-type (eql 'textures)))
  "tex")


(defmethod prepare-extension ((extension-type (eql 'textures)) owner path)
  (let ((%temp-texture-descriptors (au:dict #'eq))
        (%temp-texture-profiles (au:dict #'eq)))
    (declare (special %temp-texture-descriptors %temp-texture-profiles))
    (flet ((%prepare ()
             (load-extensions extension-type path)
             (values %temp-texture-profiles %temp-texture-descriptors)))

      (multiple-value-bind (profiles texdescs) (%prepare)
        ;; The order doesn't matter. we can type check the texture-descriptors
        ;; after reading _all_ the available textures extensions.

        ;; Process all defined profiles.
        (au:maphash-values (lambda (profile)
                             (%add-texture-profile profile owner))
                           profiles)

        ;; Process all texture-descriptors
        (au:maphash-values (lambda (texdesc)
                             (%add-texture-descriptor texdesc owner))
                           texdescs)))))


(defun resolve-all-textures (core-state)
  "This is called after all the textures are loaded in the extensions.
Ensure that these aspects of texture profiles and desdcriptors are ok:
1. The FL.TEXTURES:DEFAULT-PROFILE exists.
2. Each texture-descriptor has an updated applied-profile set of attributes.
3. All currently known about texture descriptors have valid profile references.
4. All images specified by paths actually exist at that path.
5. The texture type is valid."

  (symbol-macrolet ((profiles (profiles (textures core-state)))
                    (default-profile-name
                      (au:ensure-symbol 'default-profile 'fl.textures)))

    ;; 1. Check for fl.textures:default-profile
    (unless (au:href profiles default-profile-name)
      (error "Default-profile for texture descriptors is not defined."))

    ;; 2. For each texture-descriptor, apply all the profiles in order.
    ;; 3. Check that the specified profiles are valid.
    (au:maphash-values
     (lambda (texdesc)
       (let* ((profile-overlays
                ;; First, gather the specified profile-overlays
                (loop :for profile-overlay-name
                        :in (profile-overlay-names texdesc)
                      :collect (au:if-found (concrete-profile
                                             (au:href profiles profile-overlay-name))
                                            concrete-profile
                                            (error "Texture profile ~A does not exist."
                                                   profile-overlay-name))))

              (profile-overlays
                ;; Then, if we don't see a profile-default in there, we
                ;; put it first automatically.
                (if (member default-profile-name
                            (profile-overlay-names texdesc))
                    profile-overlays
                    (list* (au:href profiles default-profile-name)
                           profile-overlays))))

         ;; Now, overlay them left to right into the applied-attributes table
         ;; in texdesc.
         (loop :for profile-overlay :in profile-overlays
               :do (maphash (lambda (key val)
                              (setf (au:href (applied-attributes texdesc) key)
                                    val))
                            (attributes profile-overlay)))

         ;; And finally fold in the texdesc attributes last
         (maphash (lambda (key val)
                    (setf (au:href (applied-attributes texdesc) key) val))
                  (attributes texdesc))))

     (texture-descriptors (textures core-state)))

    ;; TODO: 4
    ;; TODO: 5

    nil))

;; public API
(defun general-volume-data-descriptor (&key width height depth internal-format
                                         pixel-format pixel-type data)
  "Produce a descriptor for generalized volumetric data to be loaded into a
:texture-3d type texture. If :data has the value :empty, allocate the memory of
the size and types specified on the GPU."
  ;; TODO: Implement me!
  (declare (ignore width height depth internal-format pixel-format pixel-type
                   data))
  nil)

;; public API
(defun general-planar-data-descriptor (&key width height internal-format
                                         pixel-format pixel-type data)
  "Produce a descriptor for generalized plane data to be loaded into a
:texture-2d type texture. If :data has the value :empty, allocate the memory of
the size and types specified on the GPU."
  ;; TODO: Implement me!
  (declare (ignore width height internal-format pixel-format pixel-type
                   data))
  nil)


;; Interim use of the RCACHE API.

(defmethod rcache-layout ((entry-type (eql :texture)))
  '(equalp))

(defmethod rcache-construct ((entry-type (eql :texture)) (core-state core-state)
                             &rest keys)
  (destructuring-bind (texture-location) keys
    (load-texture (context core-state) texture-location)))

(defmethod rcache-dispose ((entry-type (eql :texture)) (core-state core-state)
                           texture)
  (gl:delete-texture (texid texture)))
