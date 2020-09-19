(in-package #:virality-examples)

(v:define-prefab "default-artblob" (:library examples)
  "A base description of the damaged helmet so we can use it easily elsewhere."
  (comp:transform :rotate (q:orient :local :x o:pi/2))
  (comp:mesh :asset '(meshes art-blob)
             :name "ArtBlob")
  (comp:render :material '(x:matcap matcap/ceramic-dark
			   :uniforms ((:sampler x:matcap/ceramic-dark)))
               :slave (v:ref :self :component 'comp:mesh)))

(v:define-prefab "artblob" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("artblob" :copy "/default-artblob")
   (comp:transform :rotate/velocity (o:make-velocity v3:+forward+ (- o:pi/6))
                   :scale 4f0)))

(v:define-prefab "artblob-group" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("artblob1" :copy "/default-artblob")
   (comp:transform :rotate/velocity (o:make-velocity (v3:vec 1) (- o:pi/3))
                   :translate (v3:vec -15 0 0)
                   :scale 4f0))
  (("artblob2" :copy "/default-artblob")
   (comp:transform :translate (v3:vec 15 0 0)
                   :scale 4f0)))

(v:define-prefab "artblob-interactive" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("artblob" :copy "/default-artblob")
   (comp:transform :scale 4f0)
   (simple-mouse-rotator :clamp-p t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(v:define-prefab "default-ship01" (:library examples)
  "A base description of the damaged helmet so we can use it easily elsewhere."
  (comp:transform :rotate (q:orient :local :x o:pi/2))
  (comp:mesh :asset '(meshes ships)
             :name "Ship_001") ;; 001 to 024
  (comp:render :material '(x:matcap matcap/basic-1
			   :uniforms ((:sampler x:matcap/basic-1)))
               :slave (v:ref :self :component 'comp:mesh)))


(v:define-prefab "ship01" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("ship01" :copy "/default-ship01")
   (comp:transform :rotate/velocity (o:make-velocity v3:+forward+ (- o:pi/6))
                   :scale 4f0)))

(v:define-prefab "ship01-group" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("ship011" :copy "/default-ship01")
   (comp:transform :rotate/velocity (o:make-velocity (v3:vec 1) (- o:pi/3))
                   :translate (v3:vec -15 0 0)
                   :scale 4f0))
  (("ship012" :copy "/default-ship01")
   (comp:transform :translate (v3:vec 15 0 0)
                   :scale 4f0)))

(v:define-prefab "ship01-interactive" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("ship01" :copy "/default-ship01")
   (comp:transform :scale 4f0)
   (simple-mouse-rotator :clamp-p t)))
