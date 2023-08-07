(in-package #:virality.resource-cache)

(defstruct (cache-domain
            (:constructor %make-cache-domain)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; The domain identifier under which this cache stores unique items. A domain
  ;; value is anything that can compare under EQUAL. Usually symbols are
  ;; used. Domains represent a kind of association between a key index and a
  ;; value.
  domain

  ;; This represents a list of test functions used in a path to the deepest
  ;; nested hash table stored in the cache slot. Set upon construction. The
  ;; only valid test functions which may occur in this list are EQ, EQL, EQUAL,
  ;; and EQUALP.
  (layout nil :type (or cons null))

  ;; How many times something has been inserted into this cache.
  (inserts 0 :type integer)

  ;; How many times we removed something from the cache
  (removes 0 :type integer)

  ;; How many cache hits happened. (Counts of returning a value when it was
  ;; already present.)
  (hits 0 :type integer)

  ;; How many times we looked up something not in the cache.
  (misses 0 :type integer)

  ;; The cache of (possibly nested) hash tables. The layout indicates (up to)
  ;; the depth of hash table that gets constructed to hold elements in this
  ;; domain. Key(s) is appropriate to the domain. Value is held in CPU memory.
  (cache nil :type (or hash-table null)))

(defstruct (resource-cache
            (:constructor %make-resource-cache)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; All of the cache-domains managed by the resource cache.
  ;; Key is domain id. Value is a cache-domain object.
  (domains (u:dict #'equal) :type hash-table))
