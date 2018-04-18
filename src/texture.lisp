(in-package :fl.core)

;; this class is currently unused, but it should be!
(defclass texture ()
  (;; The texture id.
   (%texid :reader texid
           :initarg :texid)

   (%image :reader image
           :initarg :image)

   ;; stuff for material dsl
   (%name :reader name
          :initarg :name)
   (%source :reader source
            :initarg :source)
   (%sampler :accessor sampler
             :initarg :sampler)))

(defclass texture-descriptor ()
  ((%name :reader name
          :initarg :name)
   (%texture-type :reader texture-type
                  :initarg :texture-type)
   (%default-profile :reader default-profile
                     :initarg :default-profile)
   (%parameters :reader parameters
                :initarg :parameters
                :initform (au:dict #'eq))))

(defun make-texture-descriptor (&rest init-args)
  (apply #'make-instance 'texture-descriptor init-args))





;; TODO: Convert to taking a texture-decriptor instead of a location.
;; TODO: Make this return a texture object for the right kind of texture.
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


;; Interim use of the RCACHE API.

(defmethod rcache-load ((entry-type (eql :texture)) (core-state core-state)
                        texture-location &key)
  (let ((texture-id (load-texture (context core-state) texture-location)))
    texture-id))

(defmethod rcache-unload ((entry-type (eql :texture)) (core-state core-state)
                          texture-location texture-id &key)
  (gl:delete-texture texture-id))



(defmacro deftexdesc (&body body)
  ;; TODO: Note make policy optional, set to :default-policy if not specified.
  (multiple-value-bind (name textype profile true-body)
      (if (symbolp (first body))
          ;; maybe we can name the texture descriptor....
          (destructuring-bind (name (textype profile) . true-body) body
            (values name textype profile true-body))
          ;; or make it anonymous
          (destructuring-bind ((textype profile) . true-body) body
            (values nil textype profile true-body)))

    ;; all macro variables are now valid.

    (assert (zerop (mod (length true-body) 2)))
    (let ((texdesc (gensym "TEXDESC")))
      `(let ((,texdesc (make-texture-descriptor
                        :name ',name
                        :texture-type ',textype
                        :default-profile ',profile)))
         ;; Now, fill in the specified parameters
         (setf
          ,@(loop :for (key value) :on true-body :by #'cddr :appending
                  `((au:href (parameters ,texdesc) ,key) ,value)))

         ;; TODO: If there is a name, store it into an extensions hash table....
         ,texdesc))))


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
(deftexdesc name (:texture-2d default-profile)
  ;; the default-profile for texture parameters is what is used above, you can
  ;; override it here.

  :generate-mipmaps-p t ;; must only have a single base image. error otherwise
  :flip-y t
  :images #("path/to/texture.tga" ;; mipmap 0, or base image, always
            ))
