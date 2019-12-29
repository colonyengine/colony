(in-package #:cl-user)

(defpackage #:virality.nicknames
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl #:ccl
   #+(or ecl abcl clasp) #:ext
   #:add-package-local-nickname)
  (:export #:define-nicknames))

(in-package #:virality.nicknames)

(golden-utils:eval-always
  (defparameter *package-nicknames*
    '((:alexandria :a)
      (:golden-utils :u)
      (:origin :o)
      (:origin.swizzle :~)
      (:origin.vec2 :v2)
      (:origin.vec3 :v3)
      (:origin.vec4 :v4)
      (:origin.mat2 :m2)
      (:origin.mat3 :m3)
      (:origin.mat4 :m4)
      (:origin.quat :q)
      (:verbose :log)
      (:virality.actions :action)
      (:virality.colliders :col)
      (:virality.components.actions :c/action)
      (:virality.components.camera :c/cam)
      (:virality.components.camera.following :c/gcam)
      (:virality.components.camera.tracking :c/tcam)
      (:virality.components.collider :c/col)
      (:virality.components.mesh.dynamic :c/dmesh)
      (:virality.components.mesh.static :c/smesh)
      (:virality.components.render :c/render)
      (:virality.components.sprite :c/sprite)
      (:virality.components.transform :c/xform)
      (:virality.engine :v)
      (:virality.extensions.actions :x/action)
      (:virality.extensions.materials :x/mat)
      (:virality.extensions.textures :x/tex)
      (:virality.geometry :geo)
      (:shadow :gpu)
      (:virality.image :img)
      (:virality.input :in)
      (:virality.materials :mat)
      (:virality.prefabs :prefab)
      (:umbra.color :shd/color)
      (:umbra.effects :shd/fx)
      (:umbra.graphing :shd/graph)
      (:umbra.hashing :shd/hash)
      (:umbra.noise :shd/noise)
      (:umbra.sdf :shd/sdf)
      (:umbra.shaping :shd/shape)
      (:umbra.sprite :shd/sprite)
      (:virality.shaders.texture :shd/tex)
      (:virality.shaders.visualization :shd/vis)
      (:virality.textures :tex)
      (:virality.region :reg)
      (:virality.models :mod)
      (:virality.file.gltf :gltf))))

(macrolet ((define-nicknames/internal ()
             `(progn
                ,@(mapcan
                   (lambda (x)
                     (mapcar
                      (lambda (y)
                        `(add-package-local-nickname ,@(reverse y) ,(car x)))
                      *package-nicknames*))
                   (remove-if-not
                    (lambda (x)
                      (search "VIRALITY" x))
                    *package-nicknames*
                    :key (lambda (x) (symbol-name (car x))))))))
  (define-nicknames/internal))

(defmacro define-nicknames (&body body)
  `(progn
     ,@(mapcan
        (lambda (x)
          (mapcar
           (lambda (y)
             `(add-package-local-nickname ,@(reverse y) ,(car x)))
           (append *package-nicknames* body)))
        body)))
