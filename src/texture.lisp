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
                :initform (au:dict #'eq))))

;; TODO candidate for public API
(defun make-texture-profile (&rest init-args)
  (apply #'make-instance 'texture-profile init-args))


(defclass texture-descriptor ()
  ((%name :reader name
          :initarg :name)
   (%texture-type :reader texture-type
                  :initarg :texture-type)
   (%default-profile :reader default-profile
                     :initarg :default-profile)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (au:dict #'eq))))

;; TODO candidate for public API
(defun make-texture-descriptor (&rest init-args)
  (apply #'make-instance 'texture-descriptor init-args))




;; this class is currently unused, but it should be!
(defclass texture ()
  (;; The descriptr from when we derived this texture.
   (%descriptor :reader descriptor
                :initarg :descriptor)

   ;; The allocated opengl texture id.
   (%texid :reader texid
           :initarg :texid)))






;; TODO: Convert to taking a texture-decriptor instead of a location.
;; TODO: Make this return a texture object for the right kind of texture
;; that also holds the texture-id and the texture desdcriptor from when
;; it came.
;; The texture object should get cached properly and ensure that is true.
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
        id))))


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



(defmacro define-texture (name (textype profile) &body body)
  (let ((texdesc (gensym "TEXDESC")))
    `(let ((,texdesc (make-texture-descriptor
                      :name ',name
                      :texture-type ',textype
                      :default-profile ',profile)))
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


(defun validate-texture-definitions (core-state)
  "Ensure that these aspects of texture profiles and desdcriptors are ok:
1. The FL.TEXTURES:DEFAULT-PROFILE exists.
2. All currently known about texture descriptors have valid profile references.
3. All images specified by paths actually exist at that path.
4. The texture type is valid."
  nil)



;; Interim use of the RCACHE API.

(defmethod rcache-layout ((entry-type (eql :texture)))
  '(equalp))

(defmethod rcache-construct ((entry-type (eql :texture)) (core-state core-state)
                             &rest keys)
  (destructuring-bind (texture-location) keys
    (let ((texture-id (load-texture (context core-state) texture-location)))
      texture-id)))

(defmethod rcache-dispose ((entry-type (eql :texture)) (core-state core-state)
                           texture-id)
  (gl:delete-texture texture-id))
