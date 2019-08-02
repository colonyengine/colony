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

(defmacro define-nicknames (&body body)
  `(progn
     ,@(mapcan
        (lambda (x)
          (destructuring-bind (source . mappings) x
            (mapcar
             (lambda (x)
               `(add-package-local-nickname ,@x ,source))
             mappings)))
        body)))

(define-nicknames
  (:first-light
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v2 :origin.vec2)
   (:v3 :origin.vec3)
   (:comp :first-light.components))

  (:first-light.actions
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v4 :origin.vec3)
   (:q :origin.quat)
   (:v :first-light)
   (:comp :first-light.components))

  (:first-light.annotations
   (:v :first-light))

  (:first-light.components
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:~ :origin.swizzle)
   (:v2 :origin.vec2)
   (:v3 :origin.vec3)
   (:v4 :origin.vec4)
   (:m4 :origin.mat4)
   (:q :origin.quat)
   (:v :first-light))

  (:first-light.gpu
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v :first-light))

  (:first-light.materials
   (:v2 :origin.vec2)
   (:v3 :origin.vec3)
   (:v4 :origin.vec4)
   (:m4 :origin.mat4)
   (:v :first-light))

  (:first-light.prefab
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v :first-light)
   (:comp :first-light.components))

  (:first-light.textures
   (:v4 :origin.vec4)
   (:v :first-light))

  (:first-light.shader.swizzle
   (:a :alexandria)
   (:u :golden-utils))

  (:first-light.shader.user
   (:u :golden-utils)))
