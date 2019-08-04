(in-package #:virality.engine)

(defgeneric rcache-layout (entry-type)
  (:method (entry-type)
    '(eql)))

(defgeneric rcache-lookup (context entry-type &rest keys))

(defgeneric rcache-construct (context entry-type &rest keys))

(defgeneric rcache-remove (context entry-type &rest keys))

(defgeneric rcache-dispose (context entry-type removed-value))
