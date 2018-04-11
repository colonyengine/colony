(in-package :fl.core)

(defclass texture ()
  ((%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data)
   ;; stuff for material dsl
   (%name :reader name
          :initarg :name)
   (%source :reader source
            :initarg :source)
   (%sampler :accessor sampler
             :initarg :sampler)
   (%location :reader location
              :initarg :location)))

(defun get-pixel-format (channels)
  (ecase channels
    (1 :red)
    (3 :bgr)
    (4 :bgra)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    (:bgr :rgb8)
    (:bgra :rgba8)))

(defun read-texture (context location)
  (let* ((core-state (core-state context))
         (path (find-resource core-state location))
         (image (tga:read-tga path))
         (pixel-format (get-pixel-format (tga:image-channels image))))
    (make-instance 'texture
                   :width (tga:image-width image)
                   :height (tga:image-height image)
                   :pixel-format pixel-format
                   :internal-format (get-internal-format pixel-format)
                   :pixel-type :unsigned-byte
                   :location location
                   :data (tga:image-data image))))

(defun load-texture (context location &key
                                        (filter-min :linear-mipmap-linear)
                                        (filter-mag :linear)
                                        (wrap :repeat)
                                        (wrap-s wrap)
                                        (wrap-t wrap))
  (with-slots (%width %height %internal-format %pixel-format %pixel-type %data)
      (read-texture context location)
    (let ((id (gl:gen-texture)))
      (gl:bind-texture :texture-2d id)
      (gl:tex-image-2d :texture-2d 0 %internal-format %width %height 0
                       %pixel-format %pixel-type %data)
      (gl:generate-mipmap :texture-2d)
      (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
      (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
      (gl:tex-parameter :texture-2d :texture-min-filter filter-min)
      (gl:tex-parameter :texture-2d :texture-mag-filter filter-mag)
      id)))


;; Interim use of the RCACHE API.

(defmethod rcache-load ((entry-type (eql :texture)) (core-state core-state)
                        texture-location &key)
  (let ((texture-id (load-texture (context core-state) texture-location)))
    texture-id))

(defmethod rcache-unload ((entry-type (eql :texture)) (core-state core-state)
                          texture-location texture-id &key)
  (gl:delete-texture texture-id))



;; TODO Not done.
;; experimental texture descriptor work.
(defmacro deftexdesc (&body body)
  ;; TODO: Note make policy optional, set to :default-policy if not specified.
  (multiple-value-bind (name target policy true-body)
      (if (symbolp (first body))
	  ;; maybe we can name the texture descriptor....
	  (destructuring-bind (name (target policy) . true-body) body
	    (values name target policy true-body))
	  ;; or make it anonymous
	  (destructuring-bind ((target policy) . true-body) body
	    (values nil target policy true-body)))

    ;; all variables are now valid.
    `(progn
       (list :name ',name :target ',target :policy ',policy :body ',true-body))))

;; TODO Not done. define a texture parameter profile. MAybe rename.
(defmacro deftexdesc-profile (&body args)
  `(progn (list ',args)))


;; Define a profile to provide defaults for texture parameters.
(deftexdesc-profile default-profile
  ;; texparameter stuff, opengl defaults
  :depth-stencil-texture-mode :depth-component ;; note: ogl 4.3 or greater
  :texture-base-level 0
  :texture-border-color :todo-add-local-nicknames #++(v4:make 0.0 0.0 0.0 0.0)
  :texture-compare-func :never ;; unknown default
  :texture-compare-mode :none ;; unknown default
  :texture-min-filter :nearest-mipmap-linear
  :texture-mag-filter :linear
  :texture-min-lod -1000
  :texture-max-lod 1000
  :texture-max-level 1000
  :texture-swizzle-r :red
  :texture-swizzle-g :green
  :texture-swizzle-b :blue
  :texture-swizzle-a :alpha
  :texture-wrap-s :repeat
  :texture_wrap-t :repeat
  :texture-wrap-r :repeat)



;; example texture descriptor, This one is named, 'name', probably
;; fl.textures:name, but they can also be anonymous when used directly in the
;; material DSL.  It is probably good that this be a real macro or function. I
;; would change the syntax to make it easier to make a function. The materials
;; DSL will evaluate the values for the uniforms, so this could be either.
(deftexdesc name (:2d default-profile)
  ;; the default-profile for texture parameters is what is used above, you can
  ;; override it here.

  :generate-mipmaps-p t ;; must only have a single base image. error otherwise
  :flip-y t
  :images #("path/to/texture.tga" ;; mipmap 0, or base image, always
	    ))
