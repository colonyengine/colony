(in-package #:colony.texture)

;; Implementation of TEXTURE-DESCRIPTOR

;; TODO candidate for public API
(defun make-texture-descriptor (&rest init-args)
  (apply #'make-instance 'texture-descriptor init-args))

;; TODO: Candidate for public API.
(defun copy-texture-descriptor (texdesc)
  (let ((new-texdesc (make-texture-descriptor)))
    (setf
     ;; These are currently symbols.
     (name new-texdesc) (name texdesc)
     (texture-type new-texdesc) (texture-type texdesc)
     ;; This is a list
     (profile-overlay-names new-texdesc)
     (copy-seq (profile-overlay-names texdesc)))
    ;; Then copy over the attributes, we support SIMPLE values such as: string,
    ;; array, list, vector, and symbol.
    (u:do-hash (key value (attributes texdesc))
      (setf (u:href (attributes new-texdesc) key)
            (u:copy-sequence-tree value)))
    (u:do-hash (key value (applied-attributes texdesc))
      (setf (u:href (applied-attributes new-texdesc) key)
            (u:copy-sequence-tree value)))
    new-texdesc))
