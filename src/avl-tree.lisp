;; This code is not yet actually compiled into V, but it will be soon.  I placed
;; it here so we didn't lose it. It needs integration into V properly.  It's
;; main intention is to be used as the component holders for sorted rendering
;; components. We might expand its use for other things too, as needed.
;;
;; psilord

(in-package #:cl-user)

(defpackage #:avl-tree
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:min
   #:max
   #:tree)
  (:export
   #:delete
   #:find
   #:insert
   #:make-tree
   #:max
   #:min
   #:tree
   #:walk))

(in-package #:avl-tree)

(defstruct (tree (:constructor %make-tree)
                 (:predicate nil)
                 (:copier nil))
  sentinel
  root
  item-type
  (key #'identity :type function)
  (sorter #'< :type function)
  (hash-test #'eql :type function))

(u:define-printer (tree stream :identity t :type nil)
  (format stream "AVL-TREE"))

(defstruct (node (:constructor %make-node)
                 (:predicate nil)
                 (:copier nil))
  tree
  key
  data
  parent
  left
  right
  (height 0 :type fixnum)
  (balance-factor 0 :type fixnum))

(u:define-printer (node stream :identity t :type nil)
  (format stream "AVL-TREE-NODE"))

(u:fn-> node-p ((or node null)) (or node null))
(declaim (inline node-p))
(defun node-p (node)
  (declare (optimize speed))
  (unless (and node (eq node (tree-sentinel (node-tree node))))
    node))

(u:fn-> valid-p (tree) boolean)
(defun valid-p (tree)
  (declare (optimize speed))
  (let ((previous nil))
    (labels ((%check (node sorter)
               (declare (function sorter))
               (when (node-p node)
                 (when (or (null (%check (node-left node) sorter))
                           (and previous
                                (funcall sorter
                                         (node-key node)
                                         (node-key previous))))
                   (return-from %check))
                 (setf previous node)
                 (return-from %check
                   (%check (node-right node) sorter)))
               t))
      (%check (tree-root tree) (tree-sorter tree)))))

(u:fn-> make-tree (&key (:item-type symbol)
                        (:key function)
                        (:sort function)
                        (:hash-test function))
        tree)
(defun make-tree (&key item-type (key #'identity) (sort #'<) (hash-test #'eql))
  (declare (optimize speed))
  (unless item-type
    (error "Must specify :ITEM-TYPE denoting the type of items stored in the ~
            tree."))
  (let* ((tree (%make-tree
                :item-type item-type
                :key key
                :sorter sort
                :hash-test hash-test))
         (sentinel (make-node tree nil)))
    (setf (tree-sentinel tree) sentinel
          (tree-root tree) sentinel)
    tree))

(u:fn-> make-node (tree t) node)
(defun make-node (tree item)
  (declare (optimize speed))
  (let ((sentinel (tree-sentinel tree))
        (node (%make-node
               :tree tree
               :key (when item (funcall (tree-key tree) item))
               :data (u:dict (tree-hash-test tree) item item))))
    (setf (node-left node) sentinel
          (node-right node) sentinel)
    node))

(u:fn-> walk (tree function) null)
(defun walk (tree func)
  (declare (optimize speed))
  (a:when-let ((node (node-p (tree-root tree))))
    (loop :with current = node
          :with stack
          :do (cond
                ((node-p current)
                 (push current stack)
                 (setf current (node-left current)))
                (stack
                 (setf current (pop stack))
                 (u:do-hash-keys (k (node-data current))
                   (funcall func k))
                 (setf current (node-right current)))
                (t (loop-finish))))))

(u:fn-> transplant (node node) node)
(defun transplant (node1 node2)
  (declare (optimize speed))
  (let ((parent (node-parent node1)))
    (cond
      ((not (node-p parent))
       (setf (tree-root (node-tree node1)) node2))
      ((eq node1 (node-left parent))
       (setf (node-left parent) node2))
      (t (setf (node-right parent) node2)))
    (setf (node-parent node2) (node-parent node1))))

(u:fn-> rotate/left (node) node)
(defun rotate/left (node)
  (declare (optimize speed))
  (let ((p (node-parent node))
        (b (node-right node)))
    (setf (node-right node) (node-left b))
    (when (node-p (node-left b))
      (setf (node-parent (node-left b)) node))
    (setf (node-left b) node
          (node-parent node) b
          (node-parent b) p)
    (when (node-p p)
      (if (eq (node-right p) node)
          (setf (node-right p) b)
          (setf (node-left p) b)))
    (if (zerop (node-balance-factor b))
        (setf (node-balance-factor b) -1
              (node-balance-factor node) 1)
        (setf (node-balance-factor b) 0
              (node-balance-factor node) 0))
    b))

(u:fn-> rotate/right (node) node)
(defun rotate/right (node)
  (declare (optimize speed))
  (let ((p (node-parent node))
        (b (node-left node)))
    (setf (node-left node) (node-right b))
    (when (node-p (node-right b))
      (setf (node-parent (node-right b)) node))
    (setf (node-right b) node
          (node-parent node) b
          (node-parent b) p)
    (when (node-p p)
      (if (eq (node-left p) node)
          (setf (node-left p) b)
          (setf (node-right p) b)))
    (if (zerop (node-balance-factor b))
        (setf (node-balance-factor b) 1
              (node-balance-factor node) -1)
        (setf (node-balance-factor b) 0
              (node-balance-factor node) 0))
    b))

(u:fn-> rotate/left-right (node) node)
(defun rotate/left-right (node)
  (declare (optimize speed))
  (let* ((z (node-left node))
         (new-root (node-right z))
         (new-root-balance (node-balance-factor new-root)))
    (rotate/left z)
    (rotate/right node)
    (case new-root-balance
      (-1 (setf (node-balance-factor node) 1
                (node-balance-factor z) 0))
      (0 (setf (node-balance-factor node) 0
               (node-balance-factor z) 0))
      (1 (setf (node-balance-factor node) 0
               (node-balance-factor z) -1)))
    (setf (node-balance-factor new-root) 0)
    new-root))

(u:fn-> rotate/right-left (node) node)
(defun rotate/right-left (node)
  (declare (optimize speed))
  (let* ((z (node-right node))
         (new-root (node-left z))
         (new-root-balance (node-balance-factor new-root)))
    (rotate/right z)
    (rotate/left node)
    (case new-root-balance
      (-1 (setf (node-balance-factor node) 0
                (node-balance-factor z) 1))
      (0 (setf (node-balance-factor node) 0
               (node-balance-factor z) 0))
      (1 (setf (node-balance-factor node) -1
               (node-balance-factor z) 0)))
    (setf (node-balance-factor new-root) 0)
    new-root))

(u:fn-> insert-rebalance (node) (or node null))
(defun insert-rebalance (new)
  (declare (optimize speed))
  (loop :with child = new
        :for node = (node-parent child)
        :while (node-p node)
        :do (if (eq child (node-left node))
                (ecase (decf (node-balance-factor node))
                  (0 (return))
                  (-1 (setf child node))
                  (-2 (let ((node-parent (node-parent node))
                            (new-root (if (= (node-balance-factor child) 1)
                                          (rotate/left-right node)
                                          (rotate/right node))))
                        (if (node-p node-parent)
                            (return)
                            (return new-root)))))
                (ecase (incf (node-balance-factor node))
                  (0 (return))
                  (1 (setf child node))
                  (2 (let ((node-parent (node-parent node))
                           (new-root (if (= (node-balance-factor child) -1)
                                         (rotate/right-left node)
                                         (rotate/left node))))
                       (setf (node-parent new-root) node-parent)
                       (if (node-p node-parent)
                           (return)
                           (return new-root))))))))

(u:fn-> insert (tree t) node)
(defun insert (tree item)
  (declare (optimize speed))
  (a:if-let ((node (node-p (nth-value 1 (find tree item)))))
    (progn
      (setf (u:href (node-data node) item) item)
      node)
    (let ((node (make-node tree item)))
      (loop :with sorter = (tree-sorter tree)
            :with key = (node-key node)
            :with current = (tree-root tree)
            :with parent = (tree-sentinel tree)
            :while (node-p current)
            :do (setf parent current)
                (if (funcall sorter key (node-key current))
                    (setf current (node-left current))
                    (setf current (node-right current)))
            :finally (setf (node-parent node) parent)
                     (cond
                       ((not (node-p parent))
                        (setf (tree-root tree) node))
                       ((funcall sorter key (node-key parent))
                        (setf (node-left parent) node))
                       (t (setf (node-right parent) node))))
      (a:when-let ((new-root (insert-rebalance node)))
        (setf (tree-root tree) new-root))
      node)))

(u:fn-> delete-rebalance (node keyword) (or node null))
(defun delete-rebalance (new-root direction)
  (declare (optimize speed))
  (loop :for first-time = t :then nil
        :with child = new-root
        :for node = (node-parent child)
        :while (node-p node)
        :do (if (and (or (not first-time) (and first-time (eq direction :left)))
                     (eq child (node-left node)))
                (ecase (incf (node-balance-factor node))
                  (0 (setf child node))
                  (1 (return))
                  (2 (let ((node-parent (node-parent node))
                           (right-child (node-right node)))
                       (if (= (node-balance-factor right-child) -1)
                           (setf child (rotate/right-left node))
                           (setf child (rotate/left node)))
                       (setf (node-parent child) node-parent)
                       (cond
                         ((not (node-p node-parent))
                          (return child))
                         ((= (node-balance-factor right-child) -1)
                          (return))))))
                (ecase (decf (node-balance-factor node))
                  (0 (setf child node))
                  (-1 (return))
                  (-2 (let ((node-parent (node-parent node))
                            (left-child (node-left node)))
                        (if (= (node-balance-factor left-child) 1)
                            (setf child (rotate/left-right node))
                            (setf child (rotate/right node)))
                        (cond
                          ((not (node-p node-parent))
                           (return child))
                          ((= (node-balance-factor left-child) 1)
                           (return)))))))))

(u:fn-> delete (tree t) (or node null))
(defun delete (tree item)
  (declare (optimize speed))
  (labels ((%delete (node)
             (if (and (node-p (node-left node))
                      (node-p (node-right node)))
                 (let ((replacement (min (node-right node))))
                   (setf (node-data node) (node-data replacement))
                   (%delete replacement))
                 (let ((direction (if (eq node (node-left (node-parent node)))
                                      :left
                                      :right)))
                   (cond ((node-p (node-left node))
                          (transplant node (node-left node))
                          (delete-rebalance (node-left node) direction))
                         ((node-p (node-right node))
                          (transplant node (node-right node))
                          (delete-rebalance (node-right node) direction))
                         (t (transplant node (node-left node))
                            (delete-rebalance (node-left node) direction)))))))
    (a:when-let ((node (node-p (nth-value 1 (find tree item)))))
      (if (<= (hash-table-count (node-data node)) 1)
          (progn
            (a:when-let ((new-root (%delete node)))
              (setf (tree-root tree) new-root))
            (setf (node-parent (tree-sentinel tree))
                  (tree-sentinel tree)))
          (remhash item (node-data node)))
      node)))

(u:fn-> find (tree t) (values &optional t node))
(defun find (tree item)
  (declare (optimize speed))
  (labels ((%find (node key sorter)
             (declare (function sorter))
             (a:when-let ((result (and (node-p node) (node-key node))))
               (cond
                 ((funcall sorter key result)
                  (%find (node-left node) key sorter))
                 ((funcall sorter result key)
                  (%find (node-right node) key sorter))
                 (t node)))))
    (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (when (typep item (tree-item-type tree))
        (a:when-let ((node (%find (tree-root tree)
                                  (funcall (tree-key tree) item)
                                  (tree-sorter tree))))
          (values (u:href (node-data node) item)
                  node))))))

(u:fn-> min (node) (or node null))
(defun min (node)
  (declare (optimize speed))
  (when (node-p node)
    (loop :with current = node
          :for left = (node-left current)
          :while (node-p left)
          :do (setf current left)
          :finally (return current))))

(u:fn-> max (node) (or node null))
(defun max (node)
  (declare (optimize speed))
  (when (node-p node)
    (loop :with current = node
          :for right = (node-right current)
          :while (node-p right)
          :do (setf current right)
          :finally (return current))))
