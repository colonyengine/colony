(in-package #:colony-examples)

;; Experimental c:define-texture-map description.
;;
;; The c:define-texture-map DSL is a means to define how texture maps are
;; stored and organized. In this discussion, a "texture-map" is all of the data
;; associated with an opengl texture. A 2d texture map, for example, can have
;; many mipmaps associated with it. A 3d texture maps has mipmaps of a
;; different format than a 2d texture. A cube map has 6 faces, each a
;; texture-map, and each face might also have mipmaps.
;;
;;
;; In this gist, I show using the "named" form of C:DEFINE-TEXTURE-MAP and also
;; the anonymous form of (:texture-map ...) in the various examples. The use
;; of PROGN in the examples is simply to allow #++ to behave nicely since I
;; was producing this in a live lisp file and lifted it to a gist.

;; The general high level form for the named texture-map form:
;; NOTE: If NAME is NIL, then it is an anonymous texture-map.
#++
(c:define-texture-map NAME DATA-MODEL-FORM
  PROPERTY-FORM*
  DATA-FORM* which can be (depending on data-model) on of these groups:
  exactly 6 of (:FACE dirsym (DEFINE-TEXTURE-MAP ...) or texture-map-name)
  1 or more of (:MIPMAP () ...)
  1 or more of (:IMAGE () ...)
  exactly 1 of (:NAME () ...)
  form)


;; The exhaustive options for define-texture-map. Some of the data models, like
;; (:buffer <name>) and (:rect :unique) are 80% understood, but it is good to
;; implement now.
#++
(c:define-texture-map name (:single :unique) ;; The data model

  ;; The data model consists of a form: (MODEL STYLE STORE)
  ;; The MODEL is either: :single, :rect, :buffer, :voxel, :cube.
  ;; The STYLE is either: :unique, :combined.
  ;; The STORE form can vary quite a bit, be a list, and depends on
  ;; MODEL and STYLE.
  ;;
  ;; One of these data models MAY be specified (there is a default)
  ;;
  ;; () - use the default model of (:single :combined :common)
  ;;
  ;; ;; NOTE: 1d, 2d texture maps are ":single" texture maps.
  ;;  (:single :unique) - A single texture map as individual AAA mipmaps
  ;;
  ;;D (:single :combined AAA) - all mipmaps in one file (in a common format)
  ;;      AAA can be
  ;;      :common - The common packed format into one file.
  ;;      etc - Other layouts systems of the combined mipmaps in a file.
  ;;
  ;;  ;; NOTE: Rectangle storage. 2d only. Only 1 (:image () ...) form
  ;;  (:rect :unique)
  ;;
  ;;  ;; NOTE: Buffer storage, one buffer only. 1d only. No mipmap forms.
  ;;  ;; Later there is a (:name NAME)
  ;;  (:buffer :unique)
  ;;
  ;;  ;; NOTE: A 3d texture maps as unique mipmaped voxels.
  ;;  (:voxel :unique (:slices :back-to-front)) - a 3d mimap
  ;;
  ;;  ;; NOTE: cube texture maps (there are sub-texture-maps)
  ;;  (:cube :unique AAA) - cube map faces as separate texture-maps
  ;;    AAA (the syntax & semantics of the rest of the form) can be
  ;;      :six
  ;;      :opengl
  ;;    Each :face can themselves have combined or unique mipmaps.
  ;;  (:cube :combined AAA) - cube map where all faces are encoded in a
  ;;                          single image possibly under :mipmap scaling.
  ;;    AAA can be: (and there can be MANY of these)
  ;;      :auto ;; (the default) determine the combined format, even custom..
  ;;      :vcross-top
  ;;      :hcross-left
  ;;      :column
  ;;      :row
  ;;      :equirect
  ;;      (:custom <one of some unknown number and undetermined layout format>)
  ;;    Each combined image may have a mipmap of smaller combined images.

  ;; Now come one or more of these property forms: (cube maps ignore these)
  ;; :context CONTEXT - a place to automatically store anonymous texture-maps
  ;; if it is specified.
  ;;
  ;; :origin - where to consider the origin of the mipmap
  ;;   :{top,bottom}-{left,right}(-{back,front})?
  ;; D :top-left
  ;; D :bottom-left-back when :storage is (:voxel ...)!
  ;;
  ;; :channel-layout - How the channels are laid out in storage
  ;; D :combined - color channels are right next to each other
  ;;   (:plane :rgb-0) - Color planes split (only :rgb-0 supported)

  ;; Now comes 1 (:image ...), or 1 (:name ...), or 1 or more (:mipmap ...), or
  ;; 6 (face-symbol (:texture-map ...))  forms depending on the data
  ;; model. These must be last. The :image and :mipmap forms can override a
  ;; particular property for that specific image, mipmap, or texture.

  (:mipmap () (textures texture-gradient-1d)))

;; NOTE: The texture-map property of :base-level really is a property of
;; TEXTURE.


;;;
;;; Textures
;;;

(c:define-texture-map 1d-gradient (:single :unique)
  (:mipmap () (textures texture-gradient-1d)))
;; TODO: Make the define-texture return the name.
(c:define-texture 1d-gradient
    (:texture-1d x:clamp-all-edges)
  ;; TODO: TMAP Fix to accept texture-map name.
  (:data #((textures texture-gradient-1d))))

(c:define-texture-map 2d-wood (:single :unique)
  (:mipmap () (textures wood)))
(c:define-texture 2d-wood
    (:texture-2d x:clamp-all-edges)
  ;; TODO: TMAP Fix to accept texture-map name.
  (:data #((textures wood))))


(c:define-texture-map 3d (:voxel :unique (:slices :back-to-front))
  ;; mipmap level 0
  (:mipmap ()
           (textures 3d-slice-0-0)
           (textures 3d-slice-1-0)
           (textures 3d-slice-2-0)
           (textures 3d-slice-3-0)
           (textures 3d-slice-4-0)
           (textures 3d-slice-5-0)
           (textures 3d-slice-6-0)
           (textures 3d-slice-7-0))
  ;; mipmap level 1
  (:mipmap ()
           (textures 3d-slice-0-1)
           (textures 3d-slice-1-1)
           (textures 3d-slice-2-1)
           (textures 3d-slice-3-1))
  ;; mipmap level 2
  (:mipmap ()
           (textures 3d-slice-0-2)
           (textures 3d-slice-1-2))
  ;; mipmap level 3
  (:mipmap ()
           (textures 3d-slice-0-3)))
(c:define-texture 3d
    (:texture-3d x:clamp-all-edges)
  ;; TODO: Currently, these are the only valid origin and slices values. They
  ;; directly match the default of opengl.
  (:layout `((:origin :left-back-bottom)
             (:shape (:slices :back-to-front))))
  ;; TODO: Maybe I should implement pattern specification of mipmaps.
  ;; TODO: TMAP Fix to accept texture-map name
  ;; (and remove :layout in this form)
  (:data #(#((textures 3d-slice-0-0)
             (textures 3d-slice-1-0)
             (textures 3d-slice-2-0)
             (textures 3d-slice-3-0)
             (textures 3d-slice-4-0)
             (textures 3d-slice-5-0)
             (textures 3d-slice-6-0)
             (textures 3d-slice-7-0))
           #((textures 3d-slice-0-1)
             (textures 3d-slice-1-1)
             (textures 3d-slice-2-1)
             (textures 3d-slice-3-1))
           #((textures 3d-slice-0-2)
             (textures 3d-slice-1-2))
           #((textures 3d-slice-0-3)))))


(c:define-texture-map red-line (:single :unique)
  (:mipmap () (textures red-line-0))
  (:mipmap () (textures red-line-1))
  (:mipmap () (textures red-line-2))
  (:mipmap () (textures red-line-3)))

(c:define-texture-map green-line (:single :unique)
  (:mipmap () (textures green-line-0))
  (:mipmap () (textures green-line-1))
  (:mipmap () (textures green-line-2))
  (:mipmap () (textures green-line-3)))

(c:define-texture-map blue-line (:single :unique)
  (:mipmap () (textures blue-line-0))
  (:mipmap () (textures blue-line-1))
  (:mipmap () (textures blue-line-2))
  (:mipmap () (textures blue-line-3)))

(c:define-texture-map white-line (:single :unique)
  (:mipmap () (textures white-line-0))
  (:mipmap () (textures white-line-1))
  (:mipmap () (textures white-line-2))
  (:mipmap () (textures white-line-3)))

(c:define-texture 1d-array
    (:texture-1d-array x:clamp-all-edges)
  ;; If there are multiple images in each list, they are mipmaps. Since this is
  ;; a test, each mip_0 image is 8 width x 1 height
  ;; TODO: TMAP Fix to accept texture-map name
  (:data #(#((textures red-line-0)
             (textures red-line-1)
             (textures red-line-2)
             (textures red-line-3))
           #((textures green-line-0)
             (textures green-line-1)
             (textures green-line-2)
             (textures green-line-3))
           #((textures blue-line-0)
             (textures blue-line-1)
             (textures blue-line-2)
             (textures blue-line-3))
           #((textures white-line-0)
             (textures white-line-1)
             (textures white-line-2)
             (textures white-line-3)))))


(c:define-texture-map blue-fur (:single :unique)
  (:mipmap () (textures blue-fur-0))
  (:mipmap () (textures blue-fur-1))
  (:mipmap () (textures blue-fur-2))
  (:mipmap () (textures blue-fur-3))
  (:mipmap () (textures blue-fur-4))
  (:mipmap () (textures blue-fur-5))
  (:mipmap () (textures blue-fur-6))
  (:mipmap () (textures blue-fur-7))
  (:mipmap () (textures blue-fur-8))
  (:mipmap () (textures blue-fur-9))
  (:mipmap () (textures blue-fur-10)))

(c:define-texture-map bark (:single :unique)
  (:mipmap () (textures bark-0))
  (:mipmap () (textures bark-1))
  (:mipmap () (textures bark-2))
  (:mipmap () (textures bark-3))
  (:mipmap () (textures bark-4))
  (:mipmap () (textures bark-5))
  (:mipmap () (textures bark-6))
  (:mipmap () (textures bark-7))
  (:mipmap () (textures bark-8))
  (:mipmap () (textures bark-9))
  (:mipmap () (textures bark-10)))

(c:define-texture-map rock (:single :unique)
  (:mipmap () (textures rock-0))
  (:mipmap () (textures rock-1))
  (:mipmap () (textures rock-2))
  (:mipmap () (textures rock-3))
  (:mipmap () (textures rock-4))
  (:mipmap () (textures rock-5))
  (:mipmap () (textures rock-6))
  (:mipmap () (textures rock-7))
  (:mipmap () (textures rock-8))
  (:mipmap () (textures rock-9))
  (:mipmap () (textures rock-10)))

(c:define-texture-map wiggles (:single :unique)
  (:mipmap () (textures wiggles-0))
  (:mipmap () (textures wiggles-1))
  (:mipmap () (textures wiggles-2))
  (:mipmap () (textures wiggles-3))
  (:mipmap () (textures wiggles-4))
  (:mipmap () (textures wiggles-5))
  (:mipmap () (textures wiggles-6))
  (:mipmap () (textures wiggles-7))
  (:mipmap () (textures wiggles-8))
  (:mipmap () (textures wiggles-9))
  (:mipmap () (textures wiggles-10)))

(c:define-texture 2d-array
    (:texture-2d-array x:clamp-all-edges)
  ;; Since this is a test, each mip_0 image is 1024x1024 and has 11 mipmaps.
  ;; TODO: TMAP Fix to accept texture-map name
  (:data #(#((textures blue-fur-0)
             (textures blue-fur-1)
             (textures blue-fur-2)
             (textures blue-fur-3)
             (textures blue-fur-4)
             (textures blue-fur-5)
             (textures blue-fur-6)
             (textures blue-fur-7)
             (textures blue-fur-8)
             (textures blue-fur-9)
             (textures blue-fur-10))
           #((textures bark-0)
             (textures bark-1)
             (textures bark-2)
             (textures bark-3)
             (textures bark-4)
             (textures bark-5)
             (textures bark-6)
             (textures bark-7)
             (textures bark-8)
             (textures bark-9)
             (textures bark-10))
           #((textures rock-0)
             (textures rock-1)
             (textures rock-2)
             (textures rock-3)
             (textures rock-4)
             (textures rock-5)
             (textures rock-6)
             (textures rock-7)
             (textures rock-8)
             (textures rock-9)
             (textures rock-10))
           #((textures wiggles-0)
             (textures wiggles-1)
             (textures wiggles-2)
             (textures wiggles-3)
             (textures wiggles-4)
             (textures wiggles-5)
             (textures wiggles-6)
             (textures wiggles-7)
             (textures wiggles-8)
             (textures wiggles-9)
             (textures wiggles-10)))))

;; NOTE: Sample of anonymous texture-maps
(c:define-texture-map cube-map (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (textures cube-map-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (textures cube-map-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (textures cube-map-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (textures cube-map-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (textures cube-map-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (textures cube-map-front)))))

(c:define-texture cubemap (:texture-cube-map)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name
   #(((:layout :six) ;; :equirectangular, :skybox, etc, etc.
      #((:+x #((textures cube-map-right)))
        (:-x #((textures cube-map-left)))
        (:+y #((textures cube-map-top)))
        (:-y #((textures cube-map-bottom)))
        (:+z #((textures cube-map-back)))
        (:-z #((textures cube-map-front))))))))


;; Sample of fully specified texture-maps for a cube map.
(c:define-texture-map cube-map-right-2 (:single :unique)
  (:mipmap () (textures cube-map-right)))

(c:define-texture-map cube-map-left-2 (:single :unique)
  (:mipmap () (textures cube-map-left-2)))

(c:define-texture-map cube-map-top-2 (:single :unique)
  (:mipmap () (textures cube-map-top-2)))

(c:define-texture-map cube-map-bottom-2 (:single :unique)
  (:mipmap () (textures cube-map-bottom-2)))

(c:define-texture-map cube-map-back-2 (:single :unique)
  (:mipmap () (textures cube-map-back-2)))

(c:define-texture-map cube-map-front-2 (:single :unique)
  (:mipmap () (textures cube-map-front-2)))

(c:define-texture-map cube-map-2 (:cube :unique :six)
  (:face ((:dir :+x)) cube-map-right-2)
  (:face ((:dir :-x)) cube-map-left-2)
  (:face ((:dir :+y)) cube-map-top-2)
  (:face ((:dir :-y)) cube-map-bottom-2)
  (:face ((:dir :+z)) cube-map-back-2)
  (:face ((:dir :-z)) cube-map-front-2))

(c:define-texture cubemaparray (:texture-cube-map-array)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name
   #(((:layout :six)
      #((:+x #((textures cube-map-right)))
        (:-x #((textures cube-map-left)))
        (:+y #((textures cube-map-top)))
        (:-y #((textures cube-map-bottom)))
        (:+z #((textures cube-map-back)))
        (:-z #((textures cube-map-front)))))
     ((:layout :six)
      #((:+x #((textures cube-map-right-2)))
        (:-x #((textures cube-map-left-2)))
        (:+y #((textures cube-map-top-2)))
        (:-y #((textures cube-map-bottom-2)))
        (:+z #((textures cube-map-back-2)))
        (:-z #((textures cube-map-front-2))))))))


;;; Materials

(c:define-material 1d-gradient
  (:shader ex/shd:unlit-texture-1d
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 '1d-gradient)
    (:mix-color (v4:ones)))))

(c:define-material 2d-wood
  (:shader shd:unlit-texture
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 '2d-wood)
    (:mix-color (v4:ones)))))

(c:define-material 3d
  (:shader ex/shd:unlit-texture-3d
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 '3d)
    (:mix-color (v4:ones))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (float (/ (1+ (sin (* (c:total-time context) 1.5))) 2.0) 1f0))))))

(c:define-material 1d-array
  (:shader ex/shd:unlit-texture-1d-array
   :profiles (x:u-mvpt)
   :uniforms
   ((:tex.sampler1 '1d-array)
    (:mix-color (v4:ones))
    (:num-layers 4))))

(c:define-material 2d-array
  (:shader ex/shd:unlit-texture-2d-array
   :profiles (x:u-mvpt)
   :uniforms
   ((:tex.sampler1 '2d-array)
    (:mix-color (v4:ones))
    (:uv-z (lambda (context material)
             (declare (ignore material))
             ;; make sin in the range of 0 to 1 for texture coord.
             (float (/ (1+ (sin (* (c:total-time context) 1.5))) 2.0) 1f0)))
    (:num-layers 4))))

(c:define-material 2d-sweep-input
  (:shader ex/shd:noise-2d/sweep-input
   :profiles (x:u-mvp)
   :uniforms
   ;; any old 2d texture here will do since we overwrite it with noise.
   ((:tex.sampler1 '2d-wood)
    (:tex.channel0 (v2:zero))
    (:mix-color (v4:ones)))))

(c:define-material cubemap
  (:shader ex/shd:unlit-texture-cube-map
   :profiles (x:u-mvp)
   :uniforms
   ((:tex.sampler1 'cubemap)
    (:mix-color (v4:ones)))))

(c:define-material cubemaparray
  (:shader ex/shd:unlit-texture-cube-map-array
   :profiles (x:u-mvpt)
   :uniforms
   ((:tex.sampler1 'cubemaparray)
    (:mix-color (v4:ones))
    (:cube-layer
     (lambda (context material)
       (declare (ignore material))
       ;; make sin in the range of 0 to 1 for texture coord.
       (float (/ (1+ (sin (* (c:total-time context) 1.5))) 2.0) 1f0)))
    (:num-layers 2))))

;;; Components

(c:define-component shader-sweep ()
  ((%renderer :reader renderer)
   (%material :reader material)
   (%material-retrieved-p :reader material-retrieved-p
                          :initform nil)
   (%channel0 :reader channel0
              :initform (v2:zero))))

(defmethod c:on-component-initialize ((self shader-sweep))
  (with-slots (%renderer) self
    (setf %renderer (c:component-by-type (c:actor self) 'comp:render))))

(defmethod c:on-component-update ((self shader-sweep))
  (with-slots (%material %material-retrieved-p) self
    (unless %material-retrieved-p
      (setf %material (comp:material (renderer self))
            %material-retrieved-p t))
    (u:mvlet* ((context (c:context self))
               (x y (c:get-mouse-position context)))
      (when (null x) (setf x (/ c:=window-width= 2f0)))
      (when (null y) (setf y (/ c:=window-height= 2f0)))
      (v2:with-components ((c (channel0 self)))
        ;; crappy, but good enough.
        (setf cx (float (/ x c:=window-width=) 1f0)
              cy (float (/ y c:=window-height=) 1f0)))
      ;; get a reference to the material itself (TODO: use MOP stuff to get
      ;; this right so I don't always have to get it here)
      (setf (c:uniform-ref %material :tex.channel0) (channel0 self)))))

;;; Prefabs

(c:define-prefab "texture" (:library examples :policy :new-type)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 3f0))
  (("1d-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec -4f0 3f0 0f0))
   (comp:render :material '1d-gradient
                :slave (c:ref :self :component 'comp:mesh)))
  (("2d-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec -2f0 3f0 0f0))
   (comp:render :material '2d-wood
                :slave (c:ref :self :component 'comp:mesh)))
  (("3d-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 3f0 0f0))
   (comp:render :material '3d
                :slave (c:ref :self :component 'comp:mesh)))
  (("1d-array-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec 2f0 3f0 0f0))
   (comp:render :material '1d-array
                :slave (c:ref :self :component 'comp:mesh)))
  (("2d-array-texture" :copy "/mesh")
   (comp:transform :translate (v3:vec 4f0 3f0 0f0))
   (comp:render :material '2d-array
                :slave (c:ref :self :component 'comp:mesh)))
  (("swept-input" :copy "/mesh")
   (comp:transform :translate (v3:vec -4f0 1f0 0f0))
   (comp:render :material '2d-sweep-input
                :slave (c:ref :self :component 'comp:mesh))
   (shader-sweep))
  (("cube-map" :copy "/mesh")
   (comp:transform :translate (v3:vec 0f0 -1f0 0f0)
                   :rotate (q:orient :world
                                     :x (float (asin (/ (sqrt 2))) 1f0)
                                     :z o:pi/4))
   (comp:mesh :asset '(c::meshes c::primitives)
              :name "cube")
   (comp:render :material 'cubemap
                :slave (c:ref :self :component 'comp:mesh)))
  (("cube-map-array" :copy "/mesh")
   (comp:transform :translate (v3:vec 3f0 -1f0 0f0)
                   :rotate/velocity (v3:velocity (v3:ones) o:pi/4))
   (comp:mesh :asset '(c::meshes c::primitives)
              :name "cube")
   (comp:render :material 'cubemaparray
                :slave (c:ref :self :component 'comp:mesh))))
