# Lexicon of Terms

- **abstract**: Any piece of information is *abstract* if it has been
    previously constructed (almost always at macro expansion time) and stored
    in the *metaspace*.

- **app**: The application being developed with the engine such as a game, or
    visualization, or any other program.

- **appdev**: The person(s) using the engine to develop an *app*.

- **concretized metaspace**: A *metaspace* for a production release of the app
    made with this engine. All *abstract* pieces of data are intended never to
    change (via live coding or any other reason). The *concretized metadata*
    may infact be stored as files on disk (as opposed to present in the lisp
    image) and *reified* by the engine at startup time.

- **core** or **core instance**: This is an object in the *runtime* which
    represents a running instance of an *app*. While currently there is only
    one *core* in the engine, it is intended that there could be multiple
    *cores* simultaneously executing on different threads. An example might be
    a chess server and two clients--each one represented by a *core* and being
    executed concurrently in the same lisp image. Resource are shared as much
    as possible between *cores*.

- **live coding**: A situation where an *appdev* is interacting via an editor
    (like emacs), a repl, or some other means with a running instance of the
    engine and mutating *abstract* data that the engine incorporates into
    itself at opportune times. Examples of *live coding* may include: changing
    which texture-map is being used for a texture, changing a mesh, changing
    defclass definitions, changing the definition of a Common Lisp function,
    etc. *Live coding* mutations try to be synchronized to frame boundaries
    when possible, but this cannot always be held true. If the engine is not in
    the frame portion of its *runtime*, then the time of effect of *live
    coding* changes is currently unspecified.

- **metadata**: A data structure in the lisp global environment which holds
    various kinds of *abstract* data. Also a synonym for *abstract* data.
    *Metadata* is primarily used for *live coding* and during the development
    of the *app*.

- **metaspace**: This is the global environment in the Common Lisp image and
    especially the contents of the *metadata* data structure. It is where
    *metadata* constructed by macro DSL forms during the macro expansion phase
    of lisp compilation are stored. It is *strongly* suggested that the
    *runtime*, after *reification*, never reads/writes information again in the
    *metaspace*--UNLESS *live coding* is in force. During *live coding*,
    updated *abstract* information (via the *appdev*) may flow one way from the
    *metaspace* to the *runtime*. A production release of the *app* should
    never read/write to the *metaspace* during *runtime*--EXCEPT for a
    *reification* process at *app* start. There is only one *metaspace* and it
    may be empty.

- **materialization**: Constructing or loading a piece of information (such as
    an image, audio file, etc) into the main memory of the app. Examples:
    loading a file from disk into an in memory data structure, procedurally
    generating information (such as a texture-map image or an entire level) in
    an in main memory form, etc.

- **realization**: Movement of any data (which is almost certainly
    materialized) to a peripheral's memory. Examples are audio memory, GPU
    memory, etc. The storage disk of the machine, or a virtual file system
    stored on disk can be a context of *realization*.

- **reification**: This is the process (often executed during the engine start)
    which will *reify* *abstract* data into *reified* data.

- **reified**: A piece of information is *reified* if it has been transformed
    (and often copied) from the *metaspace* into a running core instance of the
    engine. *Reified* data can be freely manipulated by the engine and only
    exists for that unique instance of that execution. Data may be *reified*
    from a *concretized metapsace* as well, which could be read from disk and
    not even exist in the global lisp environment.

- **reify**: The action of converting *abstract* data in the *metadata* to
    *reified* data in the *runtime* at engine start.

- **runtime**: This is all of the code being executed and the ephemeral state
    of the engine when the engine is started and executing. When the engine
    stops executing, there is no more *runtime*.
