(in-package #:virality.engine)

;;;; This file houses various general-purpose utilities used across Virality
;;;; Engine, that either don't belong in golden-utils, or just haven't been
;;;; cross-ported yet.

;;;; Please refrain from adding any functions to this file that do not belong;
;;;; these are general purpose utilities that could be useful in other scenarios
;;;; outside of game development. This rule will help us should we choose to
;;;; lift any of these into external libraries.

;; TODO: The following function should be called early on during engine
;; initialization. ~axion 4/7/2020
(defun initialize-rng ()
  "Initializes the PRNG's state, so that the same sequence is not generated each
time the image is started."
  (setf *random-state* (make-random-state t)))

(defun split-string (string delimiter)
  "Split `STRING` into a list of two parts; everything before the first
`DELIMITER` character, and everything after it."
  (let ((pos (position delimiter string)))
    (list (subseq string 0 pos)
          (subseq string (1+ pos)))))

(defun eql* (x y)
  "Check if `X` and `Y` are equal, by using the standard `EQL` comparator first.
If that passes, we exit early with a result of `T`. If it fails, we proceed to
test in a package-relaxed manner, by comparing only the symbol names."
  (cond
    ((eql x y) t)
    ((and (symbolp x) (symbolp y))
     (string= (symbol-name x) (symbol-name y)))))

(defun copy (object)
  "Perform a pseudo-deep copy of `OBJECT`. If `OBJECT` is a value type such as a
number or symbol, returns that object as-is. If `OBJECT` is a sequence such as a
list, string, or array, recursively copies the elements of that sequence,
returning a new sequence without shared structure. Note, that this does not
perform a full deep copy, as that is difficult to generalize and we do not need
that functionality. This will only recurse until a non-sequence is hit. This
means a sequence that has an instance of a structure-object, standard-object,
hash-table, or other non-sequence will be a terminal condition for that
recursion, and be copied by reference."
  (if (typep object 'sequence)
      (map-into (copy-seq object) #'copy object)
      object))

(defun ensure-nested-hash-table (table tests keys)
  "Walk down the nested hash table `TABLE` ensuring that we have the correct
number of hash tables made (one less than the list of keys `KEYS`) with the
correct tests, received from the list of tests `TESTS`. NOTE: The first element
of `TESTS` is the test function for `TABLE` itself."
  ;; TODO: This looks painful for performance. Oh well, we'll see if the
  ;; profiler actually cares or not. It is likely that these won't be nested
  ;; deeply. Also, the algorithm is slightly painful, but the deal is that we
  ;; can't make a hash table containing the last key, since the last key is
  ;; where we'll either look something up or store it.
  (loop :with key-count = (length keys)
        :with current = table
        :for test :in (cdr tests)
        :for key :in keys
        :for i :below key-count
        :for last = (= i (1- key-count))
        :do (unless (nth-value 1 (u:href current key))
              ;; If the key doesn't exist, we make a new hash table and store it
              ;; at the key unless it is the last entry, in which case we do
              ;; nothing.
              (unless last
                (setf (u:href current key)
                      (u:dict test))))
            ;; The key is potentially newly minted.
            (setf current (u:href current key)))
  table)
