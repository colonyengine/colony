(in-package #:colony.resource-cache)

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
   (%opaque-data :accessor opaque-data :initarg :opaque-data :initform nil)
   ;; policy can be :unlocked (generally evictable), :locked (not evictable),
   ;; etc
   (%policy :accessor policy :initarg :policy :initform :unlocked)
   ;; Can be :cold (never cached), :reserved (in cache, value not computed),
   ;; :cached (in cache and usable), or :evicted (used to be in cache, but got
   ;; evicted (and possibly replaced)).
   (%state :accessor state :initarg :state :initform :cold)
   ;; TODO: at least we have: :cl-heap, :ffi-heap, :gpu-memory, :disk, etc.
   ;; :disk is to be interpreted as something we wrote to disk.
   (%location :accessor location :initarg :location)
   ;; size in bytes of entry, if applicable.
   (%size :accessor size :initarg :size)
   ;; the actual representation of value
   (%value :accessor value :initarg :value)
   ))

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
  did

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
  ;; domain.
  ;; Key(s) is appropriate to the domain.
  ;; Value is held in CPU memory.
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
  ;; KEY: domain id.
  ;; VALUE: a cache-domain object.
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

;; This will often be derived to be a specific kind of caching type which makes
;; the protocol run smoother. It doesn't HAVE to be derived, though, as long as
;; the protocol for this base class type is also specified.
;;
;; The caching-task will go through a state machine:
;;
;; Start State         |  End State           | method performing it
;; ----------------------------------------------------------------------------
;; nil                 -> :initialized        | acquire-caching-task

;; :initialized        -> :reserved           | consider-caching-task
;; :initialized        -> :retry-reservation  | consider-caching-task
;; :initialized        -> :discarded          | consider-caching-task

;; :retry-reservation  -> :retry-reservation  | consider-caching-task
;; :retry-reservation  -> :reserved           | consider-caching-task
;; :retry-reservation  -> :discarded          | consider-caching-task

;; :reserved           -> :computed           | compute-caching-task
;; :reserved           -> :retry-computation  | compute-caching-task
;; :reserved           -> :discarded          | compute-caching-task

;; :retry-computation  -> :retry-computation  | compute-caching-task
;; :retry-computation  -> :computed           | compute-caching-task
;; :retry-computation  -> :discarded          | compute-caching-task

;; :computed           -> :finalized          | finalize-caching-task
;; :computed           -> :retry-finalization | finalize-caching-task
;; :computed           -> :discarded          | finalize-caching-task

;; :retry-finalization -> :retry-finalization | finalize-caching-task
;; :retry-finalization -> :finalized          | finalize-caching-task
;; :retry-finalization -> :discarded          | finalize-caching-task

;; :discarded          -> :disposed           | discard-caching-task
;; :discarded          -> :retry-discarding   | discard-caching-task

;; :retry-discarding   -> :retry-discarding   | discard-caching-task
;; :retry-discarding   -> :disposed           | discard-caching-task

;; :finalized          -> :disposed           | dispose-caching-task
;; :finalized          -> :retry-disposing    | dispose-caching-task

;; :retry-disposing    -> :retry-disposing    | dispose-caching-task
;; :retry-disposing    -> :disposed           | dispose-caching-task

;; :disposed           -> nil                 | release-caching-task
;;
(defclass caching-task ()
  (;; the key that ends up in the resource-cache as the lookup id for the
   ;; value this caching-task ultimately computes.
   (%key :accessor key :initarg :key)
   (%opaque-data :accessor opaque-data :initarg :opaque-data)
   (%domain-id :reader domain-id :initarg :domain-id)

   ;; One of:
   ;; :discard (keep the current cache entry and discard this caching task),
   ;; NOTE: :discard knows about reservations!
   ;;
   ;; :supersede (finish computing the value and replace the cache entry)
   ;; NOTE: If multiple tasks want to supersede the same cache entry, the
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

   ;; See table above for what this could be.
   (%state :accessor state :initarg :state :initform :reserved)

   ;; Ultimate form of the key converted into the value.
   (%value :accessor value :initarg :value)

   (%core :reader core :initarg :core)
   ))

;; This is responsible for scheduling across all warmers inserted into it.  It
;; can decide to schedule tasks in the order necessary and mix and match
;; between warmers to maintain high throughput.
(defclass resource-cache-scheduler ()
  (;; KEY is domain-id, VALUE is hash-table.
   ;; In second hash table, KEY is ref of caching task, VALUE is caching-task.
   (%unscheduled-tasks :reader unscheduled-tasks
                       :initarg :unscheduled-tasks
                       :initform (u:dict #'eql))

   ;; this also has a reference to the core that contains this object since
   ;; the warming protocol will usually need access to it.
   (%core :accessor core :initarg :core)

   ;; TODO: When init-caching-task and release-caching-task actually
   ;; recycle, make an object pool slot here.
   ))

;; KEP GOING: Fix comments about return values.

;; This is the warmer protocol. Step 0,1 likely occur right after each other in
;; the code which is producing the caching-tasks. The rest happen in the
;; EXECUTE method on the executor.

;; Step 0: Executed on main thread. Not expected to be specialized (but can be)
;;
;; Not expected to be specialized or written by the user. Ask the resource
;; scheduler for a caching-task, which we initialize, and automatically store
;; it in the resource scheduler as an unscheduled task for execution. We both
;; acquire and initialize the caching-task with the init-args in this single
;; call.
;;
;; Returns two values:
;;  The first value is the keyword symbol: :initialized
;;  The second value is the initialized caching-task.
(defgeneric acquire-caching-task (resource-cache-scheduler task-type
                                  domain-id &rest init-args))

;; Step 1: Executed on main thread. Expected to be specialized.
;;
;; This function chooses if this task is a duplicate or if the it is worth
;; isnerting into the cache. It either willl discard the catching-task or
;; allow it to proceed by getting a reservation in the resource-cache for it.
;; If we're reserving in the resource-cache, then put a new cache-item
;; into the resource cache with :reserved as the state. This prolly requires
;; locking.
;;
;; Return two values:
;;  The first value is one of: :reserved, :retry-reservation, :discarded
;;  The second value is the caching-task.
(defgeneric consider-caching-task (caching-task resource-cache-scheduler))

;; Step 2: Expected to run in thread-pool. Expected to be specialized.
;;
;; The code which computes the value of the caching-task (often from the key).
;;
;; Must return two values:
;;  The first value is one of: :computed, :retry-computation, :discarded
;;  The second value is the caching-task.
(defgeneric compute-caching-task (caching-task resource-cache-scheduler))

;; Step 3: Executed on main thread. Expected to be specialized.
;;
;; Value is actually inserted into cache and the reserved cache-item is
;; finally satisfied and availabel for use. Prolly requires locking.
;;
;; Must return two values:
;;  The first value is one of: :finalized, :retry-finalization, :discarded
;;  The second value is the caching-task.
(defgeneric finalize-caching-task (caching-task resource-cache-scheduler))

;; Step 4: Executed on main thread. Expected to be specialized.
;;
;; Discards the reservation and any does any additional work beyond disposal.
;; The user's verson of this method should almost certainly call
;; dispose-caching-task if appropriate.
;;
;; Must return two values:
;;  The first value is one of: :disposed, :retry-discarding
;;  The second value is the caching-task.
(defgeneric discard-caching-task (caching-task resource-cache-scheduler))

;; Step 5: Executed on main thread. Expected to be specialized.
;;
;; Discards the reservation and any additional work beyond disposal.
;;
;; Must return two values:
;;  The first value is one of: :dispose, :retry-disposing
;;  The second value is the caching-task.
(defgeneric dispose-caching-task (caching-task resource-cache-scheduler))

;; Step 6: Executed on main thread. Not expected to be specialized (but can be)
;;
;; Release any reference to the caching-task other than possibly storing it
;; in a pool for reuse later. The user better not be messing with it if it is
;; in the pool otherwise there will be uninteded effects.
;;
;; Returns two values:
;;  The first value MUST be: nil.
;;  The second value is T if it was recycled and NIL if not.
(defgeneric release-caching-task (caching-task resource-cache-scheduler))

;;;; --------------------------------------------------------------------------
;;;; The resource cache scheduling API
;;;; --------------------------------------------------------------------------

;; NOTE: This API still needs work! What does it do? How does it have to
;; honor threading (if at all)? What thread calls this?
(defgeneric schedule (resource-cache-scheduler &key &allow-other-keys))
;; resubmit the caching-task back into the scheduler for another go-around.
(defgeneric resubmit (resource-cache-scheduler caching-task))

;;;; --------------------------------------------------------------------------
;;;; The executor API.
;;;; --------------------------------------------------------------------------

;; The executor speaks to the scheduler and arranges for the caching-tasks to
;; be completed.
(defclass resource-cache-executor ()
  ((%core :accessor core :initarg :core)))
(defclass sequential-resource-cache-executor (resource-cache-executor) ())
(defclass concurrent-resource-cache-executor (resource-cache-executor) ())

;; NOTE: The game engine is expected to specialize this on sequential or
;; concurrent excecutors. It queries a schedule from the scheduler and them
;; forms the work necessary to complete the work. Returns how many tasks were
;; processed. It is expected that this returns when all the work is
;; finished. It return how many tasks were processed (and maybe in how much
;; time too?)
(defgeneric execute (resource-cache-executor resource-cache-scheduler))
