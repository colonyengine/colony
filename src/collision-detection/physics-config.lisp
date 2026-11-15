(in-package #:colony)

(defun make-physics-config (physics-layers collision-plan)
  "Create a PHYSICS-CONFIG instance. PHYSICS-LAYERS is a list of symbols
each representing a valid physics layer. COLLISION-PLAN is a list of
lists in the form:
 ((A (A B C ...))
  (B (D E F ...))
  (G ())
  ...)
where A collides with (A B C ...) and B collides with (D E F ...) and so on.
The example form for G represents a layer that collides with nothing.
Return the PHYSICS-CONFIG instance."
  (make-instance 'physics-config
                 :physics-layers (copy-seq physics-layers)
                 :collision-plan
                 (let ((table (u:dict #'eq)))
                   (loop :for (layer collides-with-layers) :in collision-plan
                         :do (setf (u:href table layer)
                                   (copy-seq collides-with-layers)))
                   table)))
