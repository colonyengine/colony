(in-package #:virality)

(defclass asset-spec ()
  ((%pool :reader pool
          :initarg :pool)
   (%name :reader name
          :initarg :name)
   (%path :reader path
          :initarg :path)))

(u:define-printer (asset-spec stream :type nil)
  (format stream "~s (pool: ~s)" (name asset-spec) (pool asset-spec)))

(defgeneric resolve-path (asset))

(defun find-asset-pool (name)
  (u:href =meta/asset-pools= name))

(defun make-asset-spec (pool-name base-path data)
  (destructuring-bind (name path) data
    (let* ((success nil)
           (pool (find-asset-pool pool-name))
           (base-path (uiop:ensure-directory-pathname base-path))
           (path (uiop:merge-pathnames* path base-path))
           (asset (make-instance 'asset-spec
                                 :pool pool-name
                                 :name name
                                 :path path)))
      (setf (u:href pool name) asset)
      (unwind-protect (setf success (resolve-path (list pool-name name)))
        (unless success
          (remhash name pool)))
      asset)))

(defun find-asset-spec (pool-name spec-name)
  (a:if-let ((pool (find-asset-pool pool-name)))
    (or (u:href pool spec-name)
        (error "Asset ~s not found in pool ~s." spec-name pool-name))
    (error "Asset pool ~s does not exist." pool-name)))

(defun get-asset-pool-system (pool-name)
  (let ((package-name (package-name (symbol-package pool-name))))
    (or (asdf:find-system (a:make-keyword package-name) nil)
        (error "Asset pool ~s must be defined in a package with the same name ~
                as its ASDF system."
               pool-name))))

(defun update-asset-pool (pool-name base-path asset-specs)
  (let ((pool (find-asset-pool pool-name)))
    (clrhash pool)
    (dolist (spec asset-specs)
      (make-asset-spec pool-name base-path spec))))

(defun make-asset-pool (name base-path asset-specs)
  (let ((pool (u:dict #'eq)))
    (setf (u:href =meta/asset-pools= name) pool)
    (update-asset-pool name base-path asset-specs)
    pool))

(defmacro define-asset-pool (name (&key base) &body body)
  `(if (u:href =asset-pools= ',name)
       (update-asset-pool ',name ,base ',body)
       (make-asset-pool ',name ,base ',body)))

;; (defun find-asset (context type key)
;;   (u:href (assets) type key))

(defun delete-asset (type key)
  (remhash key (u:href (assets) type)))

(defun %resolve-path (system path)
  (if =release=
      #+sbcl
      (uiop:merge-pathnames*
       path
       (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))
      #-sbcl
      (error "Release must be deployed on SBCL to load assets.")
      (asdf:system-relative-pathname system path)))

(defun resolve-system-path (path &optional (system :pyx))
  (let* ((system (asdf:find-system system))
         (path (uiop:merge-pathnames*
                path
                (uiop:ensure-directory-pathname "data")))
         (resolved-path (%resolve-path system path)))
    resolved-path))

(defmethod resolve-path ((asset list))
  (destructuring-bind (pool-name spec-name) asset
    (let* ((spec (find-asset-spec pool-name spec-name))
           (system (get-asset-pool-system pool-name))
           (path (%resolve-path system (path spec))))
      (if (uiop:file-exists-p path)
          (values path spec)
          (error "File path not found for asset ~s of pool ~s.~%Path: ~s."
                 spec-name pool-name path)))))

(defmethod resolve-path ((asset string))
  (resolve-system-path asset :pyx))

(defmacro with-asset-cache (type key &body body)
  (a:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (assets) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (a:ensure-gethash ,key ,table (progn ,@body)))))
