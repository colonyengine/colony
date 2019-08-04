(in-package #:virality.engine)

(setf (meta 'resources) (u:dict #'equalp))

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

(defun %lookup-resource (key)
  (u:href (meta 'resources) key))

(defun make-relative-pathname (sub-path)
  (let ((components (split-sequence:split-sequence #\. sub-path)))
    (destructuring-bind (name &optional type) components
      (if type
          (make-pathname :name name :type type)
          (make-pathname :directory `(:relative ,name))))))

(defun build-resource-path (id key sub-path)
  (a:if-let ((base-path (%lookup-resource key)))
    (progn
      (when (pathname-type base-path)
        (error "~s is a file resource and cannot merge the sub-path ~s."
               key sub-path))
      (uiop/pathname:merge-pathnames*
       (make-relative-pathname (string-trim "/" sub-path))
       base-path))
    (error "Resource identifier ~s has not been defined yet when attempting to ~
            merge resource ~s."
           key id)))

(defun build-resource-path/2 (id path-spec)
  (destructuring-bind (key sub-path) path-spec
    (build-resource-path id key sub-path)))

(defun build-resource-path/3 (id path-spec)
  (destructuring-bind (root base sub-path) path-spec
    (let ((key (make-resource-key base root)))
      (build-resource-path id key sub-path))))

(defun make-resource-key (id root)
  (cond
    ((eq id :project)
     id)
    ((keywordp root)
     (list root id))
    (t
     id)))

(defun make-resource-path (id root path-spec)
  (flet ((make-path ()
           (case (length path-spec)
             (1 (make-relative-pathname root))
             (2 (build-resource-path/2 id path-spec))
             (3 (build-resource-path/3 id path-spec))
             (t (error "A path specifier in `define-resources` must be a ~
                        string, or a list of 2 or 3 elements."))))
         (validate-core-path (path)
           (u:when-found (key (%lookup-resource :project))
             (when (and (eq id :core)
                        (string= (namestring key) (namestring path)))
               (error "The :core and :project resource path specifications ~
                       must be unique."))))
         (validate-project-path (path)
           (u:when-found (key (%lookup-resource :core))
             (when (and (eq id :project)
                        (string= (namestring key) (namestring path)))
               (error "The :core and :project resource path specifications ~
                       must be unique.")))))
    (let ((path (make-path)))
      (validate-core-path path)
      (validate-project-path path)
      path)))

(defun store-resource-path (id path-spec)
  (unless (keywordp id)
    (error "Identifiers in `define-resources` must be keyword symbols, but ~
            found ~s."
           id))
  (let ((path-spec (a:ensure-list path-spec)))
    (destructuring-bind (root . rest) path-spec
      (declare (ignore rest))
      (let ((key (make-resource-key id root)))
        (setf (u:href (meta 'resources) key)
              (make-resource-path id root path-spec)))))
  (u:noop))

(defun get-resource-project (key)
  (let ((key (a:ensure-list key)))
    (destructuring-bind (x . rest) key
      (declare (ignore rest))
      (if (eq x :core)
          :virality.engine
          (meta 'user-project)))))

(defun resolve-resource-id (id)
  (cond
    ((eq id :project)
     id)
    ((and (keywordp id)
          (not (eq id :core)))
     (list :project id))
    (t id)))

(defmacro define-resources ((&key project) &body body)
  (let (resources)
    (unless project
      (error "Project name must be specified in a `define-resources` form."))
    (push `(setf (meta 'user-project) ,project) resources)
    (dolist (spec body)
      (destructuring-bind (id path-spec) spec
        (push `(store-resource-path ',id ',path-spec) resources)))
    `(progn
       ,@(nreverse resources))))

;;; Protocol

(defun find-resource (context resource-id &optional sub-path)
  (let* ((id (resolve-resource-id resource-id))
         (resources (resources (core context)))
         (project (get-resource-project id)))
    (u:when-found (resource (u:href resources id))
      (let ((path (uiop:merge-pathnames* sub-path resource)))
        (resolve-project-path project path)))))

(define-resources (:project :virality.engine)
  (:core "data/core")
  (:ext (:core "ext"))
  (:mesh (:core "mesh"))
  (:texture (:core "texture"))
  (:gamepad-db (:core "gamepad-db.txt"))
  (:debug-tex (:core :texture "debug.tiff")))
