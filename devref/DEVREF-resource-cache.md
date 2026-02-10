# Resource Cache and Warming Protocol

The resource cache stores (usually) deduplicated asset data in the engine and
grants multiple handles to that data. The warming protocol is an API which
allows serial or concurrent populating of the resource cache through a state
machine for which the user fills in methods for whatever type they want to put
into the resource cache. The difference between defclass and defstruct usage in
this document is thus: defclass is used when the appdev (or engine devs) is
expected to derive many new types from something.  The defstruct type is used
when derivations don't have to happen and the resulting code needs to choose
performance of convenience.

## Resource Cache

A non-exhaustive sample of kinds of data stored in the `resource-cache`:
  - images, texture-maps/mipmaps, sprite sheet data
  - mesh data
  - animation data
  - audio data
  - proceduraly generated data
  - app specific data

These are the main types in the resource cache:
```text
(defclass cache-item (lock:lockable) ...)
(defstruct cache-domain ...)
(defstruct resource-cache ...)
```

A `cache-item` is the container which has a reference to the actual item being
cached.  This allows the actual item to be swapped out without breaking the
references to the `cache-item` that the app or the engine might be holding.  It
contains some slots which can help the warming protocol plan how to load the
`cache-item`, and inform the engine where it is and how it is being used.  It
is expected that different subsystems or the appdev derive specific
`cache-item`s and extend already existing protocols to deal with them in order
to make use of the resource-cache. There are many `cache-items` types, usually
one for each kind of asset or whatever is being cached.

A `cache-domain` contains one or more `cache-item` associated with a lookup
key. A `cache-domain` is a possibly nested hash table which holds assets of
similar semantic modality--the domain.  Examples of domains could be
`:texture-maps` or `:audio`, etc. There are additional slots which perform
statistics on the hits and misses in order to help understand how the engine
performs. Note: the `layout` field describes the test functions and depth of
the hash table constituting the domain.

There is exactly one instance of a `resource-cache`. This is held in the
engine's core data structures. It contains multiple instances of
`cache-domain`s and routes cache queries to the right place to get the
associated results.

The `resource-cache` is generally layed out like this ie memory:
```text
                    +--------------+
                    |resource-cache|
                    +--------------+
                    / |  |  |  | | \
                   /  *  *  *  * *  \
                  /                  \
     +------------+                  +------------+
     |cache-domain|        ...       |cache-domain|          
     +------------+                  +------------+
     / |  |  |  | \                  / |  |  |  | \
    /  *  *  *  *  \                /  *  *  *  *  \
   /                \              /                \
+----------+     +----------+   +----------+     +----------+
|cache-item| ... |cache-item|   |cache-item| ... |cache-item|
+----------+     +----------+   +----------+     +----------+
      |      |||       |              |      |||      |
   +-----+   ***    +-----+        +-----+   ***   +-----+
   |value|          |value|        |value|         |value|
   +-----+          +-----+        +-----+         +-----+
   
```

We describe the Resource Cache API here, but realize it is likely stale
and the code itself is authoritative:
  - `cache-item` API:
    - **make-cache-item** *[Function]*
      Create a new base `cache-item` instance.
  - `cache-domain` API:
    - **make-cache-domain** *[Function]*
      Create a new `cache-domain` instance.
    - **cdref** *[Function]*
      Find a `cache-domain` by domain id.
    - **(setf cdref)** *[Function]*
      Associate a `cache-domain` with a domain id.
    - **cdrem** *[Function]*
      Remove a `cache-domain` with the given domain id.
  - `resource-cache` API:
    - **make-resource-cache** *[Function]*
      Create a `resource-cache` instance.
    - **rcref** *[Function]*
      Find the value associated with the key in a specified `cache-domain`.
    - **(setf rcref)** *[Function]*
      Set the value associated with the key in a specified `cache-domain`.
    - **rcrem** *[Function]*
      Remove the value associated with the key from the `cache-domain`.
    - **rcrefd** *[Function]*
      Return a reference to the `cache-domain` by the given domain id.
    - **rcremd** *[Function]*
      Remove the `cache-domain` specified by the gven domain id.


## Warming Protocol

KEEP GOING.























