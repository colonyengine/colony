(in-package :first-light.mfiano)

(define-material noise-test/playground
  (:profiles (u-mvpt)
   :shader fl.shader:noise-test/playground))

(define-material mesh-test
  (:profiles (u-mvp)
   :shader fl.shader:mesh-test
   :uniforms
   ((:tex.diffuse 'tank/diffuse)
    (:tex.specular 'tank/specular)
    (:tex.normal 'tank/normal)
    (:light.position (flm:vec3 0 0 1))
    (:light.color (flm:vec3 0.7))
    (:light.ambience (flm:vec3 0.05)))))
