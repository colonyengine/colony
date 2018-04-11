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
