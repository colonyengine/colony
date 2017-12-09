(in-package :fl.core)

(defun get-path (system-name &optional path)
  (if uiop/image:*image-dumped-p*
      (truename
       (uiop/pathname:merge-pathnames*
        path
        (uiop:pathname-directory-pathname (uiop:argv0))))
      (asdf/system:system-relative-pathname (make-keyword system-name) path)))

(defun map-files (path effect &key (filter (constantly t)) (recursivep t))
  (labels ((maybe-affect (file)
             (when (funcall filter file)
               (funcall effect file)))
           (process-files (dir)
             (map nil #'maybe-affect (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path)
     t recursivep #'process-files)))

(defun flatten-numbers (sequence &key (type 'single-float))
  (labels ((flatten* (object)
             (let ((list))
               (labels ((traverse (subtree)
                          (when subtree
                            (typecase subtree
                              (cons
                               (traverse (car subtree))
                               (traverse (cdr subtree)))
                              (vector
                               (map nil #'traverse subtree))
                              (t (push subtree list))))))
                 (traverse object))
               (nreverse list)))
           (coerce* (sequence)
             (mapcar
              (lambda (x) (coerce x type))
              (remove-if (complement #'realp) (flatten* sequence)))))
    (let ((sequence (coerce* sequence)))
      (make-array (length sequence)
                  :element-type type
                  :initial-contents sequence))))

(defun type-table (key type-table)
  (gethash key type-table))

(defun (setf type-table) (entry type-name-key type-table)
  (symbol-macrolet
      ((entry-ht (gethash type-name-key type-table)))
    (multiple-value-bind (looked-up-type-table presentp) entry-ht
      (unless presentp
        (let ((new-table (make-hash-table)))
          (setf entry-ht new-table
                looked-up-type-table new-table)))
      (setf (gethash entry looked-up-type-table) entry))))
