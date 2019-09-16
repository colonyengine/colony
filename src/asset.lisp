(in-package #:virality.engine)

(defun resolve-project-path (system &optional path)
  (if (and (boundp '*deployed-p*) *deployed-p*)
      #+sbcl
      (truename
       (uiop:merge-pathnames*
        path
        (uiop:pathname-directory-pathname
         (first sb-ext:*posix-argv*))))
      #-sbcl
      (error "Virality Engine was not deployed with SBCL.")
      (asdf:system-relative-pathname (asdf:find-system system) path)))

(defun find-asset (context asset &optional sub-path)
  (u:mvlet* ((project key (split-asset-key asset))
             (project (or project (project (core context))))
             (assets (assets (core context))))
    (u:if-found (path (u:href assets project key))
                (resolve-project-path
                 project
                 (uiop:merge-pathnames* sub-path path))
                (error "Asset ~s not found." asset))))

(defun check-asset-project-name (project)
  (unless (asdf:find-system project nil)
    (error "Project ~s could not be found." project)))

(defun check-asset-key-keyword (key)
  (unless (keywordp key)
    (error "Asset key is not a keyword symbol: ~s." key)))

(defun check-asset-key-leading-slash (key)
  (when (char= (char (symbol-name key) 0) #\/)
    (error "Asset key ~s cannot begin with a slash character." key)))

(defun check-asset-key-unique (table key)
  (when (u:href table key)
    (error "Duplicate asset key: ~s." key)))

(defun check-asset-key-exists (table key path-spec)
  (when (and key (not (u:href table key)))
    (error "Asset key ~s is not defined when attempting to merge the path ~
            specifier ~s."
           key path-spec)))

(defun check-asset-path-trailing-dot (path index)
  (when (and index (= (1+ index) (length path)))
    (error "Asset path ~s cannot end with a dot character." path)))

(defun check-asset-path-file-base-path (base-path path)
  (when (and base-path (pathname-type base-path))
    (error "Asset path ~s is a file and cannot be merged with path ~s."
           base-path path)))

(defun split-asset-key (key)
  (let* ((key-name (symbol-name key))
         (index (position #\/ key-name)))
    (check-asset-key-leading-slash key)
    (if index
        (values (a:make-keyword (subseq key-name 0 index))
                (a:make-keyword (subseq key-name (1+ index))))
        (values nil
                key))))

(defun split-asset-path (path)
  (let ((index (position #\. path :from-end t)))
    (check-asset-path-trailing-dot path index)
    (if index
        (values (subseq path 0 index)
                (subseq path (1+ index)))
        path)))

(defun make-asset-path (table path-spec)
  (destructuring-bind (sub-path &optional key)
      (reverse (a:ensure-list path-spec))
    (check-asset-key-exists table key path-spec)
    (u:mvlet ((base-path (print (u:href table key)))
              (name type (split-asset-path (string-trim "/" sub-path))))
      (check-asset-path-file-base-path base-path sub-path)
      (uiop:merge-pathnames*
       (if type
           (make-pathname :name name :type type)
           (make-pathname :directory `(:relative ,name)))
       base-path))))

(defun make-asset-table (spec)
  (let ((table (u:dict)))
    (u:do-plist (k v spec)
      (check-asset-key-keyword k)
      (check-asset-key-unique table k)
      (setf (u:href table k) (make-asset-path table v)))
    table))

(defmacro define-assets (project &body body)
  (let ((assets '(meta 'assets)))
    `(progn
       (check-asset-project-name ',project)
       (unless ,assets
         (setf ,assets (u:dict)))
       (setf (u:href ,assets ,project) (make-asset-table ',body)))))

(define-assets :virality.engine
  :data "data"
  :mesh (:data "mesh")
  :texture (:data "texture")
  :gamepad-db (:data "gamepad-db.txt")
  :debug-tex (:texture "debug.tiff"))
