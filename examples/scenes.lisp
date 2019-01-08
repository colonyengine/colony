(in-package :first-light.example)

;;; damaged-helmet

(fl:define-scene damaged-helmet ()
  (@camera
   ((fl.comp:camera :active-p t
                    :mode :perspective
                    :zoom 10)))
  (@damaged-helmet
   ((fl.comp:transform :rotate (flm:vec3 (/ pi 2) 0 0)
                       :rotate/inc (flm:vec3 0 0 -0.6)
                       :scale (flm:vec3 4))
    (fl.comp:mesh :location '(:mesh "damaged-helmet.glb"))
    (fl.comp:render :material 'damaged-helmet))))

;;; geometric-volumes

(fl:define-scene geometric-volumes ()
  (@camera
   ((fl.comp:camera :active-p t
                    :mode :perspective)))
  (@plane
   ((fl.comp:transform :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'fl.materials:unlit-texture)))
  (@cube
   ((fl.comp:transform :translate (flm:vec3 0 30 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:render :material 'fl.materials:unlit-texture)))
  (@sphere
   ((fl.comp:transform :translate (flm:vec3 0 -30 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "sphere.glb"))
    (fl.comp:render :material 'fl.materials:unlit-texture)))
  (@torus
   ((fl.comp:transform :translate (flm:vec3 30 0 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "torus.glb"))
    (fl.comp:render :material 'fl.materials:unlit-texture)))
  (@cone
   ((fl.comp:transform :translate (flm:vec3 -30 0 0)
                       :rotate/inc (flm:vec3 1)
                       :scale (flm:vec3 6))
    (fl.comp:mesh :location '((:core :mesh) "cone.glb"))
    (fl.comp:render :material 'fl.materials:unlit-texture-decal-bright))))

;;; graph-test

(fl:define-scene graph-test (:context context)
  (@camera
   ((fl.comp:camera :active-p t
                    :mode :orthographic)))
  (@graph
   ((fl.comp:transform :scale (flm:vec3 (/ (fl:option context :window-width) 2)
                                        (/ (fl:option context :window-height) 2)
                                        0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'graph-test))))

(fl:define-scene 3d-graph-test/1 ()
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 70 100))
    (fl.comp:camera :active-p t
                    :mode :perspective
                    :zoom 2)
    (fl.comp:tracking-camera :target-actor @graph)))
  (@graph
   ((fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(3d-graph-test
                                3d-graph-test/1
                                :shader fl.gpu.user:3d-graph-test/1
                                :instances 100000
                                :uniforms ((:size 0.5)))))))

(fl:define-scene 3d-graph-test/2 ()
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 50 100))
    (fl.comp:camera :active-p t
                    :mode :perspective
                    :zoom 2)
    (fl.comp:tracking-camera :target-actor @graph)))
  (@graph
   ((fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(3d-graph-test
                                3d-graph-test/2
                                :shader fl.gpu.user:3d-graph-test/2
                                :instances 100000
                                :uniforms ((:size 1)))))))

;;; isometric-view-test

(fl:define-scene isometric-view-test ()
  ;; The camera is specified in this specific coordinate hiearchy because we
  ;; need the rotations to compose in a very specific order.
  (@isocam-45
   ((fl.comp:transform :rotate (flm:vec3 0 0 (- (/ pi 4.0)))))
   (@isocam-35.264
    ((fl.comp:transform :rotate (flm:vec3 (- (atan (/ (sqrt 2.0)))) 0 0)))
    (@camera
     ;; translate down y axis, rotate to look at origin again.
     ((fl.comp:transform :translate (flm:vec3 0 -5 0)
                         :rotate (flm:vec3 (+ (/ pi 2.0)) 0 0))
      (fl.comp:camera :active-p t
                      :mode :orthographic
                      :zoom 100)))))
  (@plane1
   ((fl.comp:transform :translate (flm:vec3)
                       :rotate/inc (flm:vec3 (/ pi 2) (/ pi 2) (/ pi 2))
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:render :material 'fl.materials:unlit-texture)))
  (@plane2
   ((fl.comp:transform :translate (flm:vec3 2 0 0)
                       :rotate/inc (flm:vec3)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:render :material 'fl.materials:unlit-texture))))

;;; noise-test

(fl:define-scene noise-test ()
  (@camera
   ((fl.comp:camera :active-p t
                    :mode :orthographic)))
  (@perlin-3d
   ((fl.comp:transform :translate (flm:vec3 -540 202.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/perlin-3d
                                :shader fl.gpu.user:noise-test/perlin-3d))))
  (@perlin-surflet-3d
   ((fl.comp:transform :translate (flm:vec3 -325 202.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/perlin-surflet-3d
                                :shader fl.gpu.user:noise-test/perlin-surflet-3d))))
  (@perlin-improved-3d
   ((fl.comp:transform :translate (flm:vec3 -110 202.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/perlin-improved-3d
                                :shader fl.gpu.user:noise-test/perlin-improved-3d))))
  (@perlin-4d
   ((fl.comp:transform :translate (flm:vec3 110 202.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/perlin-4d
                                :shader fl.gpu.user:noise-test/perlin-4d))))
  (@cellular-3d
   ((fl.comp:transform :translate (flm:vec3 325 202.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/cellular-3d
                                :shader fl.gpu.user:noise-test/cellular-3d))))
  (@cellular-fast-3d
   ((fl.comp:transform :translate (flm:vec3 540 202.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/cellular-fast-3d
                                :shader fl.gpu.user:noise-test/cellular-fast-3d))))
  (@hermite-3d
   ((fl.comp:transform :translate (flm:vec3 -540 -22.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/hermite-3d
                                :shader fl.gpu.user:noise-test/hermite-3d))))
  (@simplex-perlin-3d
   ((fl.comp:transform :translate (flm:vec3 -325 -22.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/simplex-perlin-3d
                                :shader fl.gpu.user:noise-test/simplex-perlin-3d))))
  (@simplex-cellular-3d
   ((fl.comp:transform :translate (flm:vec3 -110 -22.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/simplex-cellular-3d
                                :shader fl.gpu.user:noise-test/simplex-cellular-3d))))
  (@simplex-polkadot-3d
   ((fl.comp:transform :translate (flm:vec3 110 -22.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/simplex-polkadot-3d
                                :shader fl.gpu.user:noise-test/simplex-polkadot-3d))))
  (@value-3d
   ((fl.comp:transform :translate (flm:vec3 325 -22.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/value-3d
                                :shader fl.gpu.user:noise-test/value-3d))))
  (@value-4d
   ((fl.comp:transform :translate (flm:vec3 540 -22.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/value-4d
                                :shader fl.gpu.user:noise-test/value-4d))))
  (@value-hermite-3d
   ((fl.comp:transform :translate (flm:vec3 -540 -247.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/value-hermite-3d
                                :shader fl.gpu.user:noise-test/value-hermite-3d))))
  (@value-perlin-3d
   ((fl.comp:transform :translate (flm:vec3 -325 -247.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/value-perlin-3d
                                :shader fl.gpu.user:noise-test/value-perlin-3d))))
  (@polkadot-3d
   ((fl.comp:transform :translate (flm:vec3 -110 -247.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/polkadot-3d
                                :shader fl.gpu.user:noise-test/polkadot-3d))))
  (@polkadot-box-3d
   ((fl.comp:transform :translate (flm:vec3 110 -247.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/polkadot-box-3d
                                :shader fl.gpu.user:noise-test/polkadot-box-3d))))
  (@cubist-3d
   ((fl.comp:transform :translate (flm:vec3 325 -247.5 0)
                       :scale (flm:vec3 90 90 0))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material '(noise-test
                                noise-test/cubist-3d
                                :shader fl.gpu.user:noise-test/cubist-3d)))))

;;; sprite-test

(fl:define-scene sprite-test ()
  (@camera
   ((fl.comp:camera :active-p t
                    :mode :orthographic)))
  (@ship
   ((fl.comp:transform :rotate (flm:vec3 0 0 (/ pi -2))
                       :scale (flm:vec3 0.5))
    (simple-movement)
    (shot-emitter))
   (@ship-body
    ((fl.comp:sprite :spec :spritesheet-data
                     :name "ship29")
     (fl.comp:render :material `(fl.materials:sprite
                                 ,(fl.util:unique-name '#:sprite)
                                 :uniforms
                                 ((:sprite.sampler sprites)))
                     :mode :sprite))
    (@exhaust
     ((fl.comp:transform :translate (flm:vec3 0 -140 0))
      (fl.comp:sprite :spec :spritesheet-data
                      :name "exhaust03-01"
                      :frames 8)
      (fl.comp:render :material `(fl.materials:sprite
                                  ,(fl.util:unique-name '#:sprite)
                                  :uniforms
                                  ((:sprite.sampler sprites)))
                      :mode :sprite)
      (fl.comp::actions :default-actions '((:type fl.actions:sprite-animate
                                            :duration 0.5
                                            :repeat-p t))))))))

(fl:define-scene sprite-test/2 ()
  (@camera
   ((fl.comp:camera :active-p t
                    :mode :orthographic)))
  (@planet
   ((fl.comp:transform :scale (flm:vec3 2))
    (fl.comp:sprite :spec :spritesheet-data
                    :name "planet04")
    (fl.comp:render :material `(fl.materials:sprite
                                ,(fl.util:unique-name '#:sprite)
                                :uniforms
                                ((:sprite.sampler sprites)))
                    :mode :sprite)
    (fl.comp::actions :default-actions '((:type fl.actions:fade-out
                                          :duration 2
                                          :repeat-p t)
                                         (:type fl.actions:rotate
                                          :duration 3
                                          :repeat-p t))))))

;;; texture-test

(fl:define-scene texture-test ()
  (@camera
   ((fl.comp:transform :translate (flm:vec3 0 0 6))
    (fl.comp:camera :active-p t
                    :mode :perspective
                    :fovy (* 90 (/ pi 180)))))
  (@plane/1d-texture
   ((fl.comp:transform :translate (flm:vec3 -4 3 0)
                       :rotate/inc (flm:vec3)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'texture-test/1d-gradient)))
  (@plane/2d-texture
   ((fl.comp:transform :translate (flm:vec3 -2 3 0)
                       :rotate (flm:vec3)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'texture-test/2d-wood)))
  (@plane/3d-texture
   ((fl.comp:transform :translate (flm:vec3 -0 3 0)
                       :rotate (flm:vec3)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'texture-test/3d-testpat)))
  (@plane/1d-array-texture
   ((fl.comp:transform :translate (flm:vec3 2 3 0)
                       :rotate (flm:vec3)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'texture-test/1d-array-testpat)))
  (@plane/2d-array-texture
   ((fl.comp:transform :translate (flm:vec3 4 3 0)
                       :rotate (flm:vec3)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'texture-test/2d-array-testarray)))
  (@plane/swept-input
   ((fl.comp:transform :translate (flm:vec3 -4 1 0)
                       :rotate (flm:vec3)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "plane.glb"))
    (fl.comp:render :material 'texture-test/2d-sweep-input)
    (shader-sweep)))
  (@cube/cube-map
   ((fl.comp:transform :translate (flm:vec3 0 -1 0)
                       :rotate (flm:vec3 0.5 0.5 0.5)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:render :material 'texture-test/testcubemap)))
  (@cube/cube-map-array
   ((fl.comp:transform :translate (flm:vec3 3 -1 0)
                       :rotate/inc (flm:vec3 0.5 0.5 0.5)
                       :scale (flm:vec3 1))
    (fl.comp:mesh :location '((:core :mesh) "cube.glb"))
    (fl.comp:render :material 'texture-test/testcubemaparray))))
