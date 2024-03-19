(in-package #:colony.texture.texture-table)

;;;; Implementation of datatype TEXTURE-TABLE

(defun make-texture-table (&rest init-args)
  (apply #'make-instance 'texture-table init-args))

(defun add-texture-profile (profile texture-table)
  (setf (u:href (profiles texture-table) (tex:name profile)) profile))

(defun remove-texture-profile (profile-name texture-table)
  (remhash profile-name (profiles texture-table)))

(defun add-semantic-texture-descriptor (texdesc texture-table)
  (setf (u:href (semantic-texture-descriptors texture-table)
                (tex:name texdesc))
        texdesc))

(defun remove-semantic-texture-descriptor (texdesc texture-table)
  (remhash texdesc (semantic-texture-descriptors texture-table)))

(defun add-unrealized-texture (texture texture-table)
  "Add a TEXTURE, which only has defined an opengl texid, to core. This book
keeps unrealized textures for the user to finalize before the game starts."
  (setf (u:href (unrealized-procedural-textures texture-table)
                (tex:name texture))
        texture))

(defun remove-unrealized-texture (texture texture-table)
  "Remove the unrealized TEXTURE from the texture tables. It is assumed that
you have just realized it by setting its opengl parameters and uploading data
or declaring storage of some kind to the GPU."
  (remhash (tex:name texture)
           (unrealized-procedural-textures texture-table)))

(defun get-unrealized-textures (texture-table)
  "Return a list of textures that are specified in materials, but haven't been
completed (no opengl parameters set for them, or data loaded into GPU).
NOTE: These are already in the resource-cache."
  (u:hash-values (unrealized-procedural-textures texture-table)))

(defun get-procedural-texture-descriptors (texture-table)
  "Return a list of all procedural texture descriptors."
  (let (results)
    (u:do-hash-values (texdesc (semantic-texture-descriptors texture-table))
      (when (eq (tex:texture-type texdesc) :procedural)
        (push texdesc results)))
    (nreverse results)))

(defun find-semantic-texture-descriptor (semantic-texture-name texture-table)
  (u:href (semantic-texture-descriptors texture-table)
          semantic-texture-name))
