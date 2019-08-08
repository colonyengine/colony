(in-package #:virality.engine)

(defun print-scene-tree (core)
  "Print an ascii representation of the scene tree indented to show children."
  (labels ((map-scene-tree (func parent &optional (level 0))
             (funcall func parent level)
             (let ((parent-transform
                     (component-by-type parent 'c/xform:transform)))
               (dolist (child (c/xform::children parent-transform))
                 (map-scene-tree func (actor child) (1+ level))))))
    (map-scene-tree
     (lambda (actor level)
       (let ((prefix-level 5))
         ;; NOTE: prefix-level is used for left justifying the level number to a
         ;; certian number of tens places for each actor. It makes the output
         ;; easier to read.
         (format t "~v@<~d~> ~v,,,v<~>Actor: ~s~%"
                 prefix-level level level #\Space (display-id actor))
         (u:do-hash-values (component (components actor))
           (format t " ~v,,,v<~> + (~(~a~):~(~a~)) [~s]~%"
                   ;; 5 for the left justified
                   (+ prefix-level level) #\space
                   (first
                    (package-nicknames
                     (symbol-package (component-type component))))
                   (component-type component) (display-id component)))))
     (scene-tree core))))
