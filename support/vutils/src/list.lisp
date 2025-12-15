(in-package #:vutils)

(defun interleave (&rest lists)
  "Interleave the elements of `LISTS`."
  (apply #'mapcan #'list lists))

(defun combinations/repetition (n items)
  "Return a list of all combinations of `ITEMS` with with a length of `N`, and
repetitions allowed."
  (if (= n 1)
      (mapcar #'list items)
      (mapcan
       (lambda (x)
         (mapcar
          (lambda (y)
            (cons x y))
          (combinations/repetition (1- n) items)))
       items)))

(defun zip (&rest lists)
  "Zip the given `LISTS`."
  (apply #'mapcar #'list lists))

(defun tree-leaves (tree test result)
  "Replace leaves of `TREE` that satisfy `TEST` with `RESULT`.
From Let Over Lambda by Doug Hoyte."
  (when tree
    (if (listp tree)
        (cons (tree-leaves (car tree) test result)
              (tree-leaves (cdr tree) test result))
        (if (funcall test tree)
            (funcall result tree)
            tree))))

(defun ragged-mapcar (void-obj func &rest lists)
  "Act just like MAPCAR when all LISTS the same size. But if the
lists are different lengths, continue processing until the longest list
is satisfied and pass VOID-OBJ to the function for any missing items of
data from those lists. VOID-OBJ must be EQL comparable."
  (loop :until (every (alexandria:rcurry #'eql void-obj) lists)
        :collecting (apply func (mapcar (lambda (lst)
                                          (if (eql lst void-obj)
                                              void-obj
                                              (car lst)))
                                        lists))
          :into result
        :do ;; We mutate the LISTS cells to keep track of the next
            ;; wavefront of items for each list. We jam VOID-OBJ into it
            ;; when we run out.
            (loop :for cell :on lists
                  :do (unless (eql (car cell) void-obj)
                        (let ((the-cdar (cdar cell)))
                          (setf (car cell)
                                (if the-cdar
                                    the-cdar
                                    void-obj)))))
        :finally (return result)))
