(in-package #:virality.resource-cache)

;;;; --------------------------------------------------------------------------
;;;; The cache-item value representation classes.
;;;; --------------------------------------------------------------------------
;;;; You don't HAVE to use these, or specific domain classes derived
;;;; from the cache-item class, as the values to insert into the resource-cache
;;;; at some key, but it will make things very convenient.

;; Base class which is often derived into domain specific caching items.
;; It is acceptable that a derived cache-item also contain references to
;; additional cache-items if needed.
(defclass cache-item ()
  (;; An opaque object the user can do whatever they want with.
   (%tag :accessor tag :initarg :tag :initform nil)
   ;; policy can be :unlocked (generally evictable), :locked (not evictable),
   ;; etc
   (%policy :accessor policy :initarg :policy :initform :unlocked)
   ;; TODO: at least we have: :cl-heap, :ffi-heap, :gpu-memory, :disk, etc.
   ;; :disk is to be interpreted as something we wrote to disk.
   (%location :accessor location :initarg :location)
   ;; size in bytes of entry, if applicable.
   (%size :accessor size :initarg :size)
   ;; the actual representation of value
   (%value :accessor value :initarg :value)
   ))

;; TODO: Move this to the higher layer that uses it.
;; The ancestral cache-item is assumed to be the GPU storage and data
;; cache-item is the main-memory storage if present.
(defclass texture-cache-item (cache-item)
  (;; If this texture is also in main memory, then data will be another
   ;; cache-item that represents this in memory data.
   (%data :accessor data :initarg :data :type (or null cache-item))))



;;;; --------------------------------------------------------------------------
;;;; The basic Cache Domain and Resource Cache API
;;;; --------------------------------------------------------------------------
;;;; The cache-domain and resource-cache API have the potential to be in the
;;;; critical path of the engine, so insead of CLOS and inheritance, we use
;;;; structures and a slight bit of manual labor. You don't have to use the
;;;; warming API described later to poke stuff into the resource-cache or
;;;; otherwise use it. But the warming API will allow you to do concurrent and
;;;; constrained insertion into the resource-cache if that is what's needed.

(defstruct (cache-domain
            (:constructor %make-cache-domain)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; The domain identifier under which this cache stores unique items. A domain
  ;; id value is anything that can compare under EQL. Usually symbols are
  ;; used. Domains represent a kind of association between a key index and a
  ;; value.
  domain-id

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


;; There is exactly one of these which represents a cache of main memory
;; objects like texture ids, or texture ids and texture memory in the case of
;; mutable procedural textures, audio buffers, component type metadata, etc.
(defstruct (resource-cache
            (:constructor %make-resource-cache)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; All of the cache-domains managed by the resource cache.
  ;; Key is domain id. Value is a cache-domain object.
  (domains (u:dict #'equal) :type hash-table))


;;;; --------------------------------------------------------------------------
;;;; The Cache Warming API
;;;; --------------------------------------------------------------------------
;;;;
;;;; This next part of the API is to help convert item names into values and
;;;; plan loading them into the resource-cache under memory constraints and
;;;; with high throughput. The API is cut up into a vertical representation to
;;;; allow high throughput concurrent resolution and loading of the desired
;;;; data into the resource-cache.

;; This doesn't get derived normally. An instance will get passed back and
;; forth through a threaded dataflow as various methods fill in each portion as
;; needed.
(defclass caching-task ()
  ( ;; the key that ends up in the resource-cache as the lookup id for the
   ;; value this caching-task ultimately computes.
   (%key :accessor key :initarg :key)
   (%opaque-data :accessor opaque-data :initarg :opaque-data)

   ;; One of:
   ;; :discard (keep the current cache entry and discard this caching task),
   ;; NOTE: :discard knows about reservations!
   ;;
   ;; :supersede (finish computing the value and replace the cache entry)
   ;; NOTE: If multiple tasks want to overwrite the same cache entry, the
   ;; last one wins. TODO: Should I record when this happens and let the main
   ;; thread know when the executor returns? Sort of seems like a situation
   ;; that we should try to minimize. Also race conditions vs live-coding and
   ;; programmatic updates by the application would be exactly the kind of
   ;; thing we'd want to observe in this race condition...
   (%if-exists :reader if-exists
               :initarg :if-exists
               :initform :discard)
   ;; One of:
   ;; :create (reserve the cache entry, compute value, fill the cache entry)
   (%if-not-exists :reader if-not-exists
                   :initarg :if-not-exists
                   :initform :create)

   ;; Set to :reserved if we processed the task and :discarded if we don't want
   ;; to process the task. The finalizer looks at this to determine what and
   ;; how much work it actually has to do.
   (%state-p :accessor state-p :initarg :state-p :initform :reserved)

   (%value :accessor value :initarg :value)

   (%core :reader core :initarg core)
   ))

;; This is responsible for scheduling across all warmers inserted into it.  It
;; can decide to schedule tasks in the order necessary and mix and match
;; between warmers to maintain high throughput.
(defclass resource-cache-scheduler ()
  (;; KEY is domain-id, VALUE is unordered linked list of caching-tasks
   (%unscheduled-tasks :reader unscheduled-tasks
                       :initarg :unscheduled-tasks
                       :initform (u:dict #'eql))

   (%core :reader core :initarg :core)

   ;; TODO: When acquire-caching-task and release-caching-task actually
   ;; recycle, make an object pool slot here.
   ))

;; This is the warmer protocol. Step 0,1 likely occur right after each other in
;; the code which is producing the caching-tasks. The rest happen in the
;; EXECUTE method on the executor.

;; executed on main thread
;; Step 0: Not expected to be specialized or written by the user.
;; Ask the resource scheduler for a caching-task which is returned by this
;; and that we will fill in.
(defgeneric acquire-caching-task (resource-cache-scheduler domain-id))

;; executed on main thread
;; Step 1: Expected to be specialized on domain-id
;; Must return the caching-task
(defgeneric init-caching-task (caching-task domain-id &key &allow-other-keys))

;; executed on main thread
;; Step 2: Expected to be specialized on domain-id
;; Mark the task if it should be discarded or not.
;; Must return the caching-task
(defgeneric reserve-or-discard-caching-task-p (caching-task domain-id))

;; These would be executed in the thread pool. It is expected that if locking
;; needs to happen here it is done here.
;; Step 3: Expected to be specialized on domain-id
;; The code which computes the value of the caching-task.
;; Must return the caching-task
(defgeneric compute-caching-task-value (caching-task domain-id))

;; executed on main thread, value is inserted into cache, or discard dealt
;; with.
;; Step 4: Expected to be specialized on domain-id
;; Usually the code that inserts the entry into the resource-cache.
;; Must return caching-task
(defgeneric finalize-caching-task (caching-task domain-id))

;; executed on main-thread
;; Step 5: Not expected to be specialized or written by user.
;; Returns T if caching-task is recycled, NIL otherwise
(defgeneric release-caching-task (resource-cache-scheduler caching-task))

;;;; --------------------------------------------------------------------------
;;;; The resource cache scheduling API
;;;; --------------------------------------------------------------------------

;; NOTE: This API still needs work! What does it do? How does it have to
;; honor threading (if at all)? What thread calls this?
(defgeneric schedule (resource-cache-scheduler &key &allow-other-keys))

;;;; --------------------------------------------------------------------------
;;;; The executor API.
;;;; --------------------------------------------------------------------------

;; The executor speaks to the scheduler and arranges for the caching-tasks to
;; be completed.
(defclass resource-cache-executor () ())
(defclass sequential-resource-cache-executor (resource-cache-executor) ())
(defclass concurrent-resource-cache-executor (resource-cache-executor) ())

;; NOTE: The game engine is expected to specialize this on sequential or
;; concurrent excecutors. It queries a schedule from the scheduler and
;; them forms the work necessary to complete the work.
(defgeneric execute (resource-cache-executor resource-cache-scheduler))
