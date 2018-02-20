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

(defun get-pixel-format (color-type)
  (ecase color-type
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:rgb :rgb8)
    (:rgba :rgba8)))

(defun read-texture (context location)
  (let* ((core-state (core-state context))
         (path (find-resource core-state location))
         (image (pngload:load-file path :flatten t :flip-y t))
         (pixel-format (get-pixel-format (pngload:color-type image))))
    (make-instance 'texture
                   :width (pngload:width image)
                   :height (pngload:height image)
                   :pixel-format pixel-format
                   :internal-format (get-internal-format pixel-format)
                   :pixel-type :unsigned-byte
                   :location location
                   :data (pngload:data image))))

(defun load-texture (context location &key
                                        (filter-min :linear-mipmap-linear)
                                        (filter-mag :linear)
                                        (wrap :clamp-to-edge))
  (with-slots (%width %height %internal-format %pixel-format %pixel-type %data)
      (read-texture context location)
    (let ((id (gl:gen-texture)))
      (gl:bind-texture :texture-2d id)
      (gl:tex-image-2d :texture-2d 0 %internal-format %width %height 0 %pixel-format %pixel-type
                       %data)
      (gl:generate-mipmap :texture-2d)
      (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
      (gl:tex-parameter :texture-2d :texture-wrap-t wrap)
      (gl:tex-parameter :texture-2d :texture-min-filter filter-min)
      (gl:tex-parameter :texture-2d :texture-mag-filter filter-mag)
      id)))
