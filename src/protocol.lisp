(in-package #:%first-light)

(defgeneric rcache-layout (entry-type))

(defgeneric rcache-lookup (context entry-type &rest keys))

(defgeneric rcache-construct (context entry-type &rest keys))

(defgeneric rcache-remove (context entry-type &rest keys))

(defgeneric rcache-dispose (context entry-type removed-value))
