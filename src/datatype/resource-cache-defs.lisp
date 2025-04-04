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
(defclass cache-item (lock:lockable)
  (;; An opaque object the user can do whatever they want with.
   (%opaque-data :accessor opaque-data :initarg :opaque-data :initform nil)
   ;; policy can be :unlocked (generally evictable), :locked (not evictable),
   ;; etc
   (%policy :accessor policy :initarg :policy :initform :unlocked)
   ;; Can be :cold (never cached), :reserved (in cache, value not
   ;; computed), :cached (in cache and usable), or :evicted (used to be
   ;; in cache, but got evicted and >this object< is not in the cache
   ;; anymore).
   (%state :accessor state :initarg :state :initform :cold)
   ;; TODO: at least we have: :cl-heap, :ffi-heap, :gpu-memory, :disk, etc.
   ;; :disk is to be interpreted as something we wrote to disk.
   (%location :accessor location :initarg :location)
   ;; size in bytes of entry, if applicable, or nil if not.
   (%size :accessor size :initarg :size :initform nil)
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
  ;; TODO: Add a lock field in here.
  ;; (lock nil)

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
;;;; data into the resource-cache. NOTE: The cache-warming protocol doesn't
;;;; necessarily have to result in anything being loaded into the
;;;; resource-cache (or even using the actual resource-cache)--it can be used
;;;; just for its state machine transitions all by itself. HOWEVER, the
;;;; specified domain will exist as a cache-domain in the resource cache.
;;;; TODO: For now, be wary that you don't leak cache-domains--maybe have a
;;;; special domain like :tmp or something that is just for temporary use?

;; This type represents where many caching-tasks will point in their INFO
;; slot. It is a "global" place for a group of caching-tasks to either
;; communicate with each other or record information about how the warming
;; protocol is going for each task. It is expected that each user of the
;; warming protocol derive an object from here specific to their needs.
(defclass warming-info (lock:lockable)
  ((%events :reader events
            :initarg :events
            ;; Key is caching-task.
            ;; Value is a list of events. An event is loosly defined.
            ;; The list is pushed onto so the head of the list is the most
            ;; recent event.
            :initform (u:dict #'eql))))

(defgeneric record-event (info caching-task event))
(defgeneric clear-events (info caching-task))
(defgeneric get-recorded-caching-tasks (info))
(defgeneric get-recorded-events (info caching-task))
(defgeneric map-events (info func))

;; This will often be derived to be a specific kind of caching type which makes
;; the protocol run smoother. It doesn't HAVE to be derived, though, as long as
;; the protocol for this base class type is also specified.
;;
;; The caching-task will go through a state machine:
;; NOTE: -c-t representes the -caching-task suffix on the method.
;;
;; NOTE: Not all methods in the protocol are represented here. Some are
;; only called internally in the indicated methods.
;;
;; Start State            |  End State              | method (w/o -c-t suffix)
;; ----------------------------------------------------------------------------
;; nil                    -> :initialized           | acquire

;; :initialized           -> :reserved              | consider
;; :initialized           -> :retry-reservation     | consider
;; :initialized           -> :discarded             | consider
;; :initialized           -> :synchronized          | consider

;; :retry-reservation     -> :retry-reservation     | consider
;; :retry-reservation     -> :reserved              | consider
;; :retry-reservation     -> :discarded             | consider

;; :reserved              -> :retry-computation     | compute
;; :reserved              -> :discarded             | compute
;; :reserved              -> :computed              | compute

;; :retry-computation     -> :retry-computation     | compute
;; :retry-computation     -> :computed              | compute
;; :retry-computation     -> :discarded             | compute

;; :computed              -> :synchronized          | synchronize-from
;; :computed              -> :retry-synchronization | synchronize-from
;; :computed              -> :discarded             | synchronize-from

;; :retry-synchronization -> :retry-synchronization | synchronize-from
;; :retry-synchronization -> :synchronized          | synchronize-from
;; :retry-synchronization -> :discarded             | synchronize-from

;; :discarded             -> :synchronized          | discard
;; :discarded             -> :retry-discarding      | discard

;; :retry-discarding      -> :retry-discarding      | discard
;; :retry-discarding      -> :synchronized          | discard

;; :synchronized          -> :disposed              | dispose
;; :synchronized          -> :retry-disposing       | dispose

;; :retry-disposing       -> :retry-disposing       | dispose
;; :retry-disposing       -> :disposed              | dispose

;; :disposed              -> nil                    | release

;; ANY                    -> :anomalous             | ANY
;; :anomalous             -> :anomalous             | rectify
;; :anomalous             -> ANY                    | rectify
;;
(defclass caching-task ()
  (;; the key that ends up in the resource-cache as the lookup id for the
   ;; value this caching-task ultimately computes.
   ;;
   ;; This value MUST BE A LIST of keys. If there is only one key, it is
   ;; in a list. This may turn out to be clunky, but we'll see.
   (%key :accessor key :initarg :key)

   ;; Untouched by the resource-cache or warming protocol.
   (%opaque-data :accessor opaque-data :initarg :opaque-data)

   ;; The domain of the caching-task.
   (%domain-id :reader domain-id :initarg :domain-id)

   ;; A warmer-info object that many caching-tasks might point to. This
   ;; allows an easy global lockable space for a group of caching-tasks
   ;; to share knowledge or record events like success/failure status in
   ;; a reliable spot.
   (%info :reader info
          :initarg :info
          :initform nil
          :type (or null warmer-info))

   ;; What do we do if the cache-item already exists in some form?
   ;; One of:
   ;;
   ;; :synchronize - This is a cache read operation. Synchronize the
   ;;                :cached cache-item into the caching-task. The
   ;;                cache-item is kept with no changes. The warming
   ;;                protocol will wait until reservations are computed
   ;;                and cached before performing synchronization with a
   ;;                caching-task.
   ;;
   ;; :nop - The associated cache-item is kept with no changes. Do
   ;;        nothing with this current caching-task (no synchronization
   ;;        in either direction or computation is performed). The
   ;;        caching-task goes straight to disposal after this.
   ;;
   ;; :supersede - This is a cache write operation. Recycle the
   ;;              associated cache-item and reserve it again. Then
   ;;              compute the caching-task and store it into the
   ;;              cache-item as expected. If multiple caching-tasks are
   ;;              attempting to perform a :supersede for the same
   ;;              :reserved cache-item, then they are ultimately
   ;;              serialized, each one replacing the newly :cached
   ;;              cache-item with their own, until last one wins.
   (%if-exists :reader if-exists
               :initarg :if-exists
               :initform :synchronize)

   ;; One of:
   ;; :create - Reserve the cache-item, compute value, fill the cache entry.
   (%if-not-exists :reader if-not-exists
                   :initarg :if-not-exists
                   :initform :create)

   ;; See table above for what this could be.
   (%state :accessor state :initarg :state)

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

;; This is the warmer protocol. Step 0,1 likely occur right after each other in
;; the code which is producing the caching-tasks. The rest happen in the
;; EXECUTE method on the executor.

;; Step 0: Executed in main thread. Not expected to be specialized (but can be)
;;
;; Not expected to be specialized or written by the user. Ask the resource
;; scheduler for a caching-task, which we initialize, and automatically store
;; it in the resource scheduler as an unscheduled task for execution. We both
;; acquire and initialize the caching-task with the init-args in this single
;; call. This method does not lock the resource-cache.
;;
;; Returns two values:
;;  The first value is one of keyword symbol: :initialized, :anomalous
;;  The second value is the initialized caching-task.
(defgeneric acquire-caching-task (resource-cache-scheduler task-type
                                  domain-id &rest init-args))

;; Step 1: Executed in thread-pool. Not expected to be specialized (but can be)
;;
;; This function chooses if this task is a cache-hit, or if it is a
;; duplicate, or if the it is worth computing and inserting into the
;; cache. This behavior depends on the policy in the caching-task
;; object. This method must locks the resource-cache.
;;
;; Return two values:
;;  The first value is one of:
;;    :reserved, :retry-reservation, :discarded, :synchronized, :anomalous
;;  The second value is the caching-task.
(defgeneric consider-caching-task (caching-task resource-cache-scheduler))

;; Step 1.2: Executed in thread-pool. May be specialized.
;;
;; This function looks up the key specified in the caching-task in the
;; resource-cache to see if there is an associated cache-item or other
;; object associated with the key. This method assumes the
;; resource-cache is locked.
;;
;; Return two values:
;;  The first value is the cache-item or other appropriate item.
;;  The second item is T if the cache-item (or other obj) was present, NIL
;;   otherwise.
(defgeneric lookup-caching-task (caching-task resource-cache-scheduler
                                 resource-cache))

;; Step 1.5a: Executed in same thread as consider-caching-task. Expected to be
;; specialized.
;;
;; This function allocates a new cache-item, sets it to :reserved, and inserts
;; it into the resource-cache. This method can assume the resource-cache is
;; already locked.
;;
;; Return two values:
;;  The first value is one of:
;;    :reserved, :anomalous
;;  The second value is the caching-task.
(defgeneric reserve-caching-task (caching-task resource-cache-scheduler
                                  resource-cache))

;; Step 1.5b: Executed in same thread as consider-caching-task. Expected to be
;; specialized.
;;
;; This function recycles the passed in cache-item for reuse. It must
;; destroy/free whatever resrouces the cache-item is using, and remark
;; it to :reserved. This method can assume the resource-cache is already
;; locked. This method must complete the recycling and reservation in
;; one call.
;;
;; Return two values:
;;  The first value is one of:
;;    :reserved, :anomalous
;;  The second value is the caching-task.
(defgeneric recycle-caching-task (caching-task cache-item
                                  resource-cache-scheduler))

;; Step 2: Executed in thread-pool. Expected to be specialized.
;;
;; The code which computes the value of the caching-task (often from the key).
;; This method does not expect to lock the resource-cache.
;;
;; Must return two values:
;;  The first value is one of:
;;    :computed, :retry-computation, :discarded, :anomalous
;;  The second value is the caching-task.
(defgeneric compute-caching-task (caching-task resource-cache-scheduler))

;; Step 3a: Executed in thread-pool. Expected to be specialized.
;;
;; The computed caching-task value is copied into the :reserved
;; cache-item and the cache-item is marked :cached and becomes available
;; for use. This method must lock the resource-cache.
;;
;; Must return two values:
;;  The first value is one of:
;;    :synchronized, :retry-synchronization, :discarded, :anomalous
;;  The second value is the caching-task.
(defgeneric synchronize-from-caching-task (caching-task
                                           resource-cache-scheduler))

;; Step 3b: Executed in thread-pool. Expected to be specialized.
;;
;; There is already a :cached cache-item for the requested key in the
;; caching-task. Copy that value into the caching-task (if needed) and
;; do whatever other book keeping is required for this value with
;; respect to the caching-task. (For example, the caching-task might
;; have higher level structures the value must be put into or other
;; associations to make, etc). This method assumes the resource-cache is
;; already locked.
;;
;; Must return two values:
;;  The first value is one of:
;;    :synchronized, :retry-synchronization, :discarded, :anomalous
;;  The second value is the caching-task.
(defgeneric synchronize-to-caching-task (caching-task cache-item
                                         resource-cache-scheduler))

;; Step 4: Executed in thread-pool. Expected to be specialized.
;;
;; Remove the :reserved or :cached cache-item associated with the
;; caching-task from the resource-cache. Destroy the contents of the
;; cache-item and free any resources it might be using. Mark the
;; cache-item :evicted and drop all references to it. This ignors the
;; eviction policy in the cache-item. This method must lock the
;; resource-cache.
;;
;; Must return two values:
;;  The first value is one of:
;;    :synchronized, :retry-synchronization, :anomalous
;;  The second value is the caching-task.
(defgeneric discard-caching-task (caching-task resource-cache-scheduler))

;; Step 5: Executed in thread-pool. Expected to be specialized.
;;
;; This method ONLY can free resources used (if any) in the caching-task
;; object. This method is not expected to lock the resource-cache.
;;
;; Must return two values:
;;  The first value is one of: :disposed, :retry-disposing, :anomalous
;;  The second value is the caching-task.
(defgeneric dispose-caching-task (caching-task resource-cache-scheduler))

;; Step 6: Executed in main thread. Not expected to be specialized (but can be)
;;
;; Release any reference to the caching-task other than possibly storing it
;; in a pool for reuse later. The user better not be messing with it if it is
;; in the pool otherwise there will be unintended effects. This method is not
;; expeted to lock the resource-cache.
;;
;; Returns two values:
;;  The first value is one of:
;;    nil, :anomalous
;;  The second value is T if it was recycled and NIL if not.
(defgeneric release-caching-task (caching-task resource-cache-scheduler))

;; Step X: Executed in threading pool. Expected to be specialized.
;;
;; For any anomalous caching-task, figure out why it is anomalous, fix
;; it, and send it to its next state. There is a little squishyness to
;; the definition of the domain this method can behave in. When we start
;; using it seriously we can figure it out then. This is expected to
;; lock the resource-cache if it needs to mess with it.
;;
;; Returns two values:
;;  The first value is one of: ANY state
;;  The second value is the caching-task.
(defgeneric rectify-caching-task (caching-task resource-cache-scheduler))

;;;; --------------------------------------------------------------------------
;;;; The resource cache scheduling API
;;;; --------------------------------------------------------------------------

;; NOTE: This API still needs work! What does it do? How does it have to
;; honor threading (if at all)? What thread calls this? WHat it does do is
;; remove all the tasks from the scheduler and return them in some order
;; or grouping for processing. When this method returns, the scheduler is
;; empty, so when moving down the state machine, we SUBMIT the partial
;; solutions back into the scheduler with SUBMIT.
(defgeneric schedule (resource-cache-scheduler &key &allow-other-keys))
;; submit the caching-task (often back) into the scheduler.
(defgeneric submit (resource-cache-scheduler caching-task))

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
