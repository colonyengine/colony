(in-package #:virality.engine)

(defstruct (avl-tree
            (:constructor %make-avl-tree)
            (:predicate nil)
            (:copier nil))
  sentinel
  root
  item-type
  (key #'identity :type function)
  (sorter #'< :type function)
  (hash-test #'eql :type function))

(u:define-printer (avl-tree stream :identity t :type nil)
  (format stream "AVL-TREE"))

(defstruct (avl-node (:constructor %make-avl-node)
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

(u:define-printer (avl-node stream :identity t :type nil)
  (format stream "AVL-TREE-NODE"))

(u:fn-> avl-node-p ((or avl-node null)) (or avl-node null))
(declaim (inline avl-node-p))
(defun avl-node-p (node)
  (declare (optimize speed))
  (unless (and node (eq node (avl-tree-sentinel (avl-node-tree node))))
    node))

(u:fn-> avl-valid-p (avl-tree) boolean)
(defun avl-valid-p (tree)
  (declare (optimize speed))
  (let ((previous nil))
    (labels ((%check (node sorter)
               (declare (function sorter))
               (when (avl-node-p node)
                 (when (or (null (%check (avl-node-left node) sorter))
                           (and previous
                                (funcall sorter
                                         (avl-node-key node)
                                         (avl-node-key previous))))
                   (return-from %check))
                 (setf previous node)
                 (return-from %check
                   (%check (avl-node-right node) sorter)))
               t))
      (%check (avl-tree-root tree) (avl-tree-sorter tree)))))

(u:fn-> make-avl-tree (&key (:item-type symbol)
                            (:key function)
                            (:sort function)
                            (:hash-test function))
        avl-tree)
(defun make-avl-tree (&key item-type (key #'identity) (sort #'<)
                        (hash-test #'eql))
  (declare (optimize speed))
  (unless item-type
    (error "Must specify :ITEM-TYPE denoting the type of items stored in the ~
            tree."))
  (let* ((tree (%make-avl-tree
                :item-type item-type
                :key key
                :sorter sort
                :hash-test hash-test))
         (sentinel (make-avl-node tree nil)))
    (setf (avl-tree-sentinel tree) sentinel
          (avl-node-left sentinel) sentinel
          (avl-node-right sentinel) sentinel
          (avl-tree-root tree) sentinel)
    (clrhash (avl-node-data (avl-tree-sentinel tree)))
    tree))

(u:fn-> make-avl-node (avl-tree t) avl-node)
(defun make-avl-node (tree item)
  (declare (optimize speed))
  (let ((sentinel (avl-tree-sentinel tree))
        (node (%make-avl-node
               :tree tree
               :key (when item (funcall (avl-tree-key tree) item))
               :data (u:dict (avl-tree-hash-test tree) item item))))
    (setf (avl-node-left node) sentinel
          (avl-node-right node) sentinel)
    node))

(u:fn-> avl-walk (avl-tree function) null)
(defun avl-walk (tree func)
  (declare (optimize speed))
  (a:when-let ((node (avl-node-p (avl-tree-root tree))))
    (loop :with current = node
          :with stack
          :do (cond
                ((avl-node-p current)
                 (push current stack)
                 (setf current (avl-node-left current)))
                (stack
                 (setf current (pop stack))
                 (u:do-hash-keys (k (avl-node-data current))
                   (funcall func k))
                 (setf current (avl-node-right current)))
                (t (loop-finish))))))

(u:fn-> avl-transplant (avl-node avl-node) avl-node)
(defun avl-transplant (node1 node2)
  (declare (optimize speed))
  (let ((parent (avl-node-parent node1)))
    (cond
      ((not (avl-node-p parent))
       (setf (avl-tree-root (avl-node-tree node1)) node2))
      ((eq node1 (avl-node-left parent))
       (setf (avl-node-left parent) node2))
      (t (setf (avl-node-right parent) node2)))
    (setf (avl-node-parent node2) (avl-node-parent node1))))

(u:fn-> avl-rotate/left (avl-node) avl-node)
(defun avl-rotate/left (node)
  (declare (optimize speed))
  (let ((p (avl-node-parent node))
        (b (avl-node-right node)))
    (setf (avl-node-right node) (avl-node-left b))
    (when (avl-node-p (avl-node-left b))
      (setf (avl-node-parent (avl-node-left b)) node))
    (setf (avl-node-left b) node
          (avl-node-parent node) b
          (avl-node-parent b) p)
    (when (avl-node-p p)
      (if (eq (avl-node-right p) node)
          (setf (avl-node-right p) b)
          (setf (avl-node-left p) b)))
    (if (zerop (avl-node-balance-factor b))
        (setf (avl-node-balance-factor b) -1
              (avl-node-balance-factor node) 1)
        (setf (avl-node-balance-factor b) 0
              (avl-node-balance-factor node) 0))
    b))

(u:fn-> avl-rotate/right (avl-node) avl-node)
(defun avl-rotate/right (node)
  (declare (optimize speed))
  (let ((p (avl-node-parent node))
        (b (avl-node-left node)))
    (setf (avl-node-left node) (avl-node-right b))
    (when (avl-node-p (avl-node-right b))
      (setf (avl-node-parent (avl-node-right b)) node))
    (setf (avl-node-right b) node
          (avl-node-parent node) b
          (avl-node-parent b) p)
    (when (avl-node-p p)
      (if (eq (avl-node-left p) node)
          (setf (avl-node-left p) b)
          (setf (avl-node-right p) b)))
    (if (zerop (avl-node-balance-factor b))
        (setf (avl-node-balance-factor b) 1
              (avl-node-balance-factor node) -1)
        (setf (avl-node-balance-factor b) 0
              (avl-node-balance-factor node) 0))
    b))

(u:fn-> avl-rotate/left-right (avl-node) avl-node)
(defun avl-rotate/left-right (node)
  (declare (optimize speed))
  (let* ((z (avl-node-left node))
         (new-root (avl-node-right z))
         (new-root-balance (avl-node-balance-factor new-root)))
    (avl-rotate/left z)
    (avl-rotate/right node)
    (case new-root-balance
      (-1 (setf (avl-node-balance-factor node) 1
                (avl-node-balance-factor z) 0))
      (0 (setf (avl-node-balance-factor node) 0
               (avl-node-balance-factor z) 0))
      (1 (setf (avl-node-balance-factor node) 0
               (avl-node-balance-factor z) -1)))
    (setf (avl-node-balance-factor new-root) 0)
    new-root))

(u:fn-> avl-rotate/right-left (avl-node) avl-node)
(defun avl-rotate/right-left (node)
  (declare (optimize speed))
  (let* ((z (avl-node-right node))
         (new-root (avl-node-left z))
         (new-root-balance (avl-node-balance-factor new-root)))
    (avl-rotate/right z)
    (avl-rotate/left node)
    (case new-root-balance
      (-1 (setf (avl-node-balance-factor node) 0
                (avl-node-balance-factor z) 1))
      (0 (setf (avl-node-balance-factor node) 0
               (avl-node-balance-factor z) 0))
      (1 (setf (avl-node-balance-factor node) -1
               (avl-node-balance-factor z) 0)))
    (setf (avl-node-balance-factor new-root) 0)
    new-root))

(u:fn-> avl-insert-rebalance (avl-node) (or avl-node null))
(defun avl-insert-rebalance (new)
  (declare (optimize speed))
  (loop :with child = new
        :for node = (avl-node-parent child)
        :while (avl-node-p node)
        :do (if (eq child (avl-node-left node))
                (ecase (decf (avl-node-balance-factor node))
                  (0 (return))
                  (-1 (setf child node))
                  (-2 (let ((node-parent (avl-node-parent node))
                            (new-root (if (= (avl-node-balance-factor child) 1)
                                          (avl-rotate/left-right node)
                                          (avl-rotate/right node))))
                        (if (avl-node-p node-parent)
                            (return)
                            (return new-root)))))
                (ecase (incf (avl-node-balance-factor node))
                  (0 (return))
                  (1 (setf child node))
                  (2 (let ((node-parent (avl-node-parent node))
                           (new-root (if (= (avl-node-balance-factor child) -1)
                                         (avl-rotate/right-left node)
                                         (avl-rotate/left node))))
                       (setf (avl-node-parent new-root) node-parent)
                       (if (avl-node-p node-parent)
                           (return)
                           (return new-root))))))))

(u:fn-> avl-insert (avl-tree t) avl-node)
(defun avl-insert (tree item)
  (declare (optimize speed))
  (a:if-let ((node (avl-node-p (nth-value 1 (avl-find tree item)))))
    (progn
      (setf (u:href (avl-node-data node) item) item)
      node)
    (let ((node (make-avl-node tree item)))
      (loop :with sorter = (avl-tree-sorter tree)
            :with key = (avl-node-key node)
            :with current = (avl-tree-root tree)
            :with parent = (avl-tree-sentinel tree)
            :while (avl-node-p current)
            :do (setf parent current)
                (if (funcall sorter key (avl-node-key current))
                    (setf current (avl-node-left current))
                    (setf current (avl-node-right current)))
            :finally (setf (avl-node-parent node) parent)
                     (cond
                       ((not (avl-node-p parent))
                        (setf (avl-tree-root tree) node))
                       ((funcall sorter key (avl-node-key parent))
                        (setf (avl-node-left parent) node))
                       (t (setf (avl-node-right parent) node))))
      (a:when-let ((new-root (avl-insert-rebalance node)))
        (setf (avl-tree-root tree) new-root))
      node)))

(u:fn-> avl-delete-rebalance (avl-node keyword) (or avl-node null))
(defun avl-delete-rebalance (new-root direction)
  (declare (optimize speed))
  (loop :for first-time = t :then nil
        :with child = new-root
        :for node = (avl-node-parent child)
        :while (avl-node-p node)
        :do (if (and (or (not first-time) (and first-time (eq direction :left)))
                     (eq child (avl-node-left node)))
                (ecase (incf (avl-node-balance-factor node))
                  (0 (setf child node))
                  (1 (return))
                  (2 (let ((node-parent (avl-node-parent node))
                           (right-child (avl-node-right node)))
                       (if (= (avl-node-balance-factor right-child) -1)
                           (setf child (avl-rotate/right-left node))
                           (setf child (avl-rotate/left node)))
                       (setf (avl-node-parent child) node-parent)
                       (cond
                         ((not (avl-node-p node-parent))
                          (return child))
                         ((= (avl-node-balance-factor right-child) -1)
                          (return))))))
                (ecase (decf (avl-node-balance-factor node))
                  (0 (setf child node))
                  (-1 (return))
                  (-2 (let ((node-parent (avl-node-parent node))
                            (left-child (avl-node-left node)))
                        (if (= (avl-node-balance-factor left-child) 1)
                            (setf child (avl-rotate/left-right node))
                            (setf child (avl-rotate/right node)))
                        (cond
                          ((not (avl-node-p node-parent))
                           (return child))
                          ((= (avl-node-balance-factor left-child) 1)
                           (return)))))))))

(u:fn-> avl-delete (avl-tree t) (or avl-node null))
(defun avl-delete (tree item)
  (declare (optimize speed))
  (labels ((%delete (node)
             (if (and (avl-node-p (avl-node-left node))
                      (avl-node-p (avl-node-right node)))
                 (let ((replacement (avl-min (avl-node-right node))))
                   (setf (avl-node-data node) (avl-node-data replacement))
                   (%delete replacement))
                 (let ((direction (if (eq node (avl-node-left
                                                (avl-node-parent node)))
                                      :left
                                      :right)))
                   (cond ((avl-node-p (avl-node-left node))
                          (avl-transplant node (avl-node-left node))
                          (avl-delete-rebalance (avl-node-left node) direction))
                         ((avl-node-p (avl-node-right node))
                          (avl-transplant node (avl-node-right node))
                          (avl-delete-rebalance (avl-node-right node)
                                                direction))
                         (t (avl-transplant node (avl-node-left node))
                            (avl-delete-rebalance (avl-node-left node)
                                                  direction)))))))
    (a:when-let ((node (avl-node-p (nth-value 1 (avl-find tree item))))
                 (sentinel (avl-tree-sentinel tree)))
      (when (u:href (avl-node-data node) item)
        (if (<= (hash-table-count (avl-node-data node)) 1)
            (progn
              (a:when-let ((new-root (%delete node)))
                (setf (avl-tree-root tree) new-root))
              (setf (avl-node-parent sentinel) sentinel)
              (clrhash (avl-node-data node)))
            (remhash item (avl-node-data node))))
      node)))

(u:fn-> avl-find (avl-tree t) (values &optional t avl-node))
(defun avl-find (tree item)
  (declare (optimize speed))
  (labels ((%find (node key sorter)
             (declare (function sorter))
             (a:when-let ((result (and (avl-node-p node) (avl-node-key node))))
               (cond
                 ((funcall sorter key result)
                  (%find (avl-node-left node) key sorter))
                 ((funcall sorter result key)
                  (%find (avl-node-right node) key sorter))
                 (t node)))))
    (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (when (typep item (avl-tree-item-type tree))
        (a:when-let ((node (%find (avl-tree-root tree)
                                  (funcall (avl-tree-key tree) item)
                                  (avl-tree-sorter tree))))
          (values (u:href (avl-node-data node) item)
                  node))))))

(u:fn-> avl-min (avl-node) (or avl-node null))
(defun avl-min (node)
  (declare (optimize speed))
  (when (avl-node-p node)
    (loop :with current = node
          :for left = (avl-node-left current)
          :while (avl-node-p left)
          :do (setf current left)
          :finally (return current))))

(u:fn-> avl-max (avl-node) (or avl-node null))
(defun avl-max (node)
  (declare (optimize speed))
  (when (avl-node-p node)
    (loop :with current = node
          :for right = (avl-node-right current)
          :while (avl-node-p right)
          :do (setf current right)
          :finally (return current))))
