(in-package #:fl.example)

(defmethod prologue (context)
  (format t "Running PROLOGUE method!~%")

  (example-0/unrealized-textures context))


(defun example-0/unrealized-textures (context)
  ;; Example 0: Unrealized Procedural Textures.
  ;; These are textures whose texture descriptors are :procedural and
  ;; also referenced in a define-material.
  ;; Goal:
  ;; A) Get the texture-descriptor for ALL unrealized textures.
  ;; For each unrealized texture:
  ;;   B) Set the opengl parameters
  ;;   C) Allocate main memory for the texture.
  ;;   D) Generate a 2d texture into that main memory.
  ;;   E) Upload it to the GPU.
  ;;   F) Free the main memory texture.
  ;;   G) Remove texture from unrealized list.
  (format T "Found unrealized textures: ~A~%"
          (%fl::get-unrealized-textures context))


  nil)
