(in-package :first-light.psilord)

(define-options ()
  :title "psilord's Playground"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug)

(define-resources (:project :first-light.psilord)
  (:project "data")
  (:ext (:project "ext"))
  (:texture (:project "textures")))

(define-texture alignment (:texture-2d)
  (:data #((:texture "alignment.tiff"))))

(defparameter +color/red+ (flm:vec4 1 0 0 1))
(defparameter +color/green+ (flm:vec4 0 1 0 1))
(defparameter +color/blue+ (flm:vec4 0 0 1 1))
(defparameter +color/white+ (flm:vec4 1))

(define-material unlit-texture-test-0
  (:shader fl.shader:unlit-texture
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'fl.textures:debug-texture)
    (:mix-color (flm:vec4 1)))))

(define-material unlit-texture-test-1
  (:shader fl.shader:unlit-texture
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:tex.sampler1 'fl.textures:debug-texture)
    (:mix-color
     ;; The uniform value is a function that returns a custom semantic value. It will be called on
     ;; EACH binding of this material, so every ON-COMPONENT-RENDER call usually.
     (lambda (context material)
       (declare (ignore context material))
       (aref #(:red :green :blue :white) (random 4)))
     ;; And then we define a function specific to this uniform to convert the custom semantic value
     ;; to something that is acceptable as a type for this uniform. The semantic-transformer is
     ;; ALWAYS run first in the composition sequence for the material.
     :transformer
     (lambda (sv context material)
       (declare (ignore context material))
       (ecase sv
         (:red +color/red+)
         (:green +color/green+)
         (:blue +color/blue+)
         (:white +color/white+)))))))

(define-material test-material-0
  (:shader fl.shader:test-shader-0
   :profiles (fl.materials:u-mvp)
   :uniforms
   ((:texes (vector 'fl.textures:debug-texture
                    'alignment))
    (:mix-color (flm:vec4 1))
    (:interpolation #(0.5 0.5 0.5 0.5)))))
