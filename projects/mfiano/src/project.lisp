(in-package :first-light.mfiano)

(define-options ()
  :title "mfiano's Playground"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug)

(define-resources (:project :first-light.mfiano)
  (:project "data")
  (:ext (:project "ext"))
  (:mesh (:project "mesh"))
  (:texture (:project "textures")))

(define-texture tank/diffuse (:texture-2d)
  (:data #((:texture "tank-diffuse.tga"))))

(define-texture tank/specular (:texture-2d)
  (:data #((:texture "tank-specular.tga"))))

(define-texture tank/normal (:texture-2d)
  (:data #((:texture "tank-normal.tga"))))

(define-material noise-test/playground
  (:profiles (fl.materials:u-mvpt)
   :shader fl.shader:noise-test/playground))

(define-material mesh-test
  (:profiles (fl.materials:u-mvp)
   :shader fl.shader:mesh-test
   :uniforms
   ((:tex.diffuse 'tank/diffuse)
    (:tex.specular 'tank/specular)
    (:tex.normal 'tank/normal)
    (:light.position (flm:vec3 0 0 1))
    (:light.color (flm:vec3 0.7))
    (:light.ambience (flm:vec3 0.05)))))
