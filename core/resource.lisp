(in-package :%fl)

;;; DSL Input

;; (define-resources (:project "project-name")
;;   (:core "data")
;;   (:mesh (:core "mesh"))
;;   (:shader (:core "shader"))
;;   (:texture (:core "texture"))
;;   (:char-meshes (:core :mesh "character"))
;;   (:debug-tex (:core :texture "debug.tga"))
;;   (:project (:core "my-project")))

;;; DSL Expansion

;; Expands to a global definition form whose value bound is the following:
;; hash table (key -> value):

;; :core -> #p"data/"
;; (:core :mesh) -> #p"data/mesh/"
;; (:core :shader) -> #p"data/shader/"
;; (:core :texture) -> #p"data/texture/"
;; (:core :char-meshes) -> #p"data/mesh/character/"
;; (:core :debug-tex) -> #p"data/texture/debug.tga"
;; :project -> #p"data/my-project/"

;;; Details:

;; 1) Resource Specifications and Identifiers

;; The body of `define-resources` is an arbitrary number of lists; each one being a resource
;; specification which describes where to locate a particular resource. A resource specification has
;; as its first element, a keyword symbol to identify the resource about to be described, and must
;; be unique with other resource identifiers.

;; In the "DSL Input" example above, :shader is an example of a resource identifier.

;; Another example in the above example is :core, which is a special resource identifier in
;; first-light. This is where all of first-light's internal resources are stored; here, they are
;; located in the "data" directory directly in the user's project working directory.

;; NOTE: The resource identifiers :core and :project have special semantics that will be described
;; progressively below.

;; NOTE: The order of defined resources is important. This is a simple single-pass parser for ease
;; of maintenance and extensibility, and therfor you cannot build a path to a resource based off of
;; another resource that is defined later in the form. This is a tolerable restriction, as all
;; resources are defined within the same form for a project, and also the user would likely order
;; them in a logical fashion rather than jump around to mentally resolve paths in a hurry, when not
;; using the resource API functions. Resource specifications are read top to bottom, and resolution
;; will fail during compilation time if this rule is not followed.

;; 2) Resource Path Specifications

;; Following the resource identifier in a resource specification is a path specification, which is
;; either a string denoting a root path, or a list denoting how to build off of a pre-existing path.

;; In the "DSL Input" example above, "data" is an example of a string path specification. This
;; denotes that the resource identified by :core is a root path located directly inside the user's
;; project directory.

;; NOTE: Users are permitted to change the path specification of :core from "data" to any string of
;; their choosing, since it is part of their working directory and they can make the required
;; changes to the filesystem within their project.

;; Apart from string path specifications, there is another form. If a path specification is a list,
;; its first element points to a pre-existing resource identifier, hereafter known as the "base
;; resource", and the second element is a string with the same semantics as a string path specifier
;; as described above. In this form, The base resource path is resolved, and the string merged onto
;; the end of the base path to form a new resource path.

;; In the example above, :shader has a base resource as :core, and a string as "shader". First, the
;; resource identifier :core is resolved to #p"data/" and then merged with "shader" to produce
;; #p"data/shader/", as can be seen in the "DSL Expansion" above.

;; There is also a third format for path specifications which is a list of three elements. In this
;; form, the second and last element behave exactly as in the two-element format. The first element
;; is used to disambiguate between :core and :project, the two special resource identifiers of
;; first-light. This is because it is completely permitted to use the same keyword resource
;; identifier in a user's project, as one already defined internal to first-light for :core.

;; In the "DSL Input" example above, :debug-tex defines a resource that makes use of the
;; three-element path specification format. Here, we can see that :debug-tex refers to the
;; "debug.tga" file located by the :texture identifier in first-light's core resource definitions,
;; and not by resolving a user-defined :texture identifier.

;; NOTE: In all three forms of path specifications, there is a string component with consistent
;; semantics. It represents a path to a resource in the filesystem. Therefor, you are permitted to
;; specify a string containing slashes ("/" characters) to point to a file or directory, such as
;; "path/to/file.txt".

;; NOTE: A string component in a path specification is always treated as a relative path. You cannot
;; prepend a slash ("/" character) to a string to force it being absolute. Relative pathnames are
;; required for Common Lisp to properly merge pathnames, and logically all paths to resources are
;; relative to the user's project. Care is taken to strip off any leading or trailing slashes from
;; supplied string components.

;; IMPORTANT: A string component in a path specification is treated as a file if anywhere in the
;; string is a dot ("." character). Paths to directories cannot contain a dot, unless you mean that
;; the last component of that path points to a file. Likewise, a path specification for a file must
;; contain a dot, so ensure that your resource all have a file extensions. This is a fair
;; restriction, as it lets us easily delineate between a file or directory in the DSL's parser,
;; which is important when merging Common Lisp pathnames.

;; IMPORTANT: first-light's :core resource path specification is not permitted to use anything other
;; than the string format, unlike other resource path specifications. Using a list form, such as
;; (:core (:project "data")) will not define first-light's core resources to be relative to the
;; user's project resources. Instead, a new core resource will be defined for the user's project,
;; since we do not enforce any restrictions on user project resource identifier naming.

;; 3) Initialization Form

;; The initialization form of the DSL input is the form directly following the macro name. In the
;; "DSL Input" example above, that would be (:project "project-name"). This is a propertly list of
;; key/value pairs. Currently, the only possible key is :project, and it is mandatory. This must
;; match the name of the ASDF system the user's project is defined in. This is used by the resource
;; resolver to get the full absolute path to a resource on disk when loading the project in a
;; development environment with ASDF.

;; NOTE: In the context of a deployed binary, paths are resolved relative to the executable.

;; 4) API

;; TODO

(defclass resource-data ()
  ((%project :accessor project)
   (%table :reader table
           :initform (fl.util:dict #'equalp))))

(defparameter *resource-data* (make-instance 'resource-data))

(defun make-relative-pathname (sub-path)
  (let ((components (fl.util:split-sequence #\. sub-path)))
    (destructuring-bind (name &optional type) components
      (if type
          (make-pathname :name name :type type)
          (make-pathname :directory `(:relative ,name))))))

(defun build-resource-path (id key sub-path)
  (fl.util:if-let ((base-path (fl.util:href (table *resource-data*) key)))
    (progn
      (when (pathname-type base-path)
        (error "~s is a file resource and cannot merge the sub-path ~s." key sub-path))
      (uiop/pathname:merge-pathnames*
       (make-relative-pathname (string-trim "/" sub-path))
       base-path))
    (error "Resource identifier ~s has not been defined yet when attempting to merge resource ~s."
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
             (t (error "A path specifier in `define-resources` must be a string, or a list of 2 or ~
                        3 elements."))))
         (validate-core/project-path (core/project path)
           (fl.util:when-found (key (fl.util:href (table *resource-data*) core/project))
             (when (and (member id '(:core :project))
                        (string= (namestring key) (namestring path)))
               (error "The :core and :project resource path specifications must be unique.")))))
    (let ((path (make-path)))
      (validate-core/project-path :core path)
      (validate-core/project-path :project path)
      path)))

(defun store-resource-path (id path-spec)
  (unless (keywordp id)
    (error "Identifiers in `define-resources` must be keyword symbols, but found ~s." id))
  (let ((path-spec (fl.util:ensure-list path-spec)))
    (destructuring-bind (root &rest rest) path-spec
      (declare (ignore rest))
      (let ((key (make-resource-key id root)))
        (setf (fl.util:href (table *resource-data*) key)
              (make-resource-path id root path-spec)))))
  (fl.util:noop))

(defun get-resource-project (key)
  (let ((key (fl.util:ensure-list key)))
    (destructuring-bind (x &rest rest) key
      (declare (ignore rest))
      (if (eq x :core)
          :first-light
          (project *resource-data*)))))

(defun resolve-resource-id (id)
  (cond
    ((eq id :project)
     id)
    ((and (keywordp id)
          (not (eq id :core)))
     (list :project id))
    (t id)))

(defmacro define-resources ((&key project) &body body)
  (unless project
    (error "Project name must be specified in a `define-resources` form."))
  `(progn
     ,@(fl.util:collecting
         (collect `(setf (project *resource-data*) ,project))
         (dolist (spec body)
           (destructuring-bind (id path-spec) spec
             (collect `(store-resource-path ',id ',path-spec)))))))

(in-package :fl)

(define-resources (:project :first-light)
  (:core "data/core")
  (:ext (:core "ext"))
  (:mesh (:core "mesh"))
  (:texture (:core "texture"))
  (:gamepad-db (:core "gamepad-db.txt"))
  (:debug-tex (:core :texture "debug.tga")))
