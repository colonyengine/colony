(in-package #:virality.engine)

(defun print-scene-tree (core)
  "Print an ascii representation of the scene tree indented to show children."
  (labels ((map-scene-tree (func parent &optional (level 0))
             (funcall func parent level)
             (let ((parent-transform
                     (component-by-type parent 'comp.transform:transform)))
               (dolist (child (comp.transform::children parent-transform))
                 (map-scene-tree func (actor child) (1+ level))))))
    (map-scene-tree
     (lambda (actor level)
       (let ((prefix-level 5))
         ;; NOTE: prefix-level is used for left justifying the level number to a
         ;; certian number of tens places for each actor. It makes the output
         ;; easier to read.
         (format t "~v@<~d~> ~v,,,v<~>Actor: ~s~%"
                 prefix-level level level #\Space (display-id actor))
         (u:do-hash-values (component (actor::components actor))
           (format t " ~v,,,v<~> + (~(~a~):~(~a~)) [~s]~%"
                   ;; 5 for the left justified
                   (+ prefix-level level) #\space
                   (first
                    (package-nicknames
                     (symbol-package (component-type component))))
                   (component-type component) (display-id component)))))
     (scene-tree core))))

(defun print-resources ()
  (flet ((compare-keys (k1 k2)
           (let* ((k1 (a:ensure-list k1))
                  (k2 (a:ensure-list k2))
                  (i (mismatch k1 k2)))
             (when i
               (cond
                 ((= i (length k1)) t)
                 ((= i (length k2)) nil)
                 (t (string< (nth i k1) (nth i k2))))))))
    (let ((keys)
          (column-width 0))
      (u:do-hash-keys (k (meta 'resources))
        (push k keys)
        (when (listp k)
          (let ((key-width (length (symbol-name (cadr k)))))
            (when (> key-width column-width)
              (setf column-width (+ key-width 14))))))
      (format t "~&~va ~a~%~%" column-width "Identifier" "Relative path")
      (dolist (k (sort keys #'compare-keys))
        (let ((v (%lookup-resource k)))
          (format t "~&~vs ~s" column-width k (namestring v)))))))
