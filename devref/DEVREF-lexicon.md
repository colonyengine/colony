# Lexicon of Terms

- **ADAPI**: "AppDev API". This represents an interface destined to be used by
the *appdev*. The engine maintainers must try to keep these APIs stable as long
as possible to keep backwards compatibility with previous versions of the
engine.

- **abstract**: Any piece of information is *abstract* if it has been
previously constructed (almost always at macro expansion time) and stored in
the *metaspace*.

- **actor**: An empty "bag" into which *components* are placed. *Actors* are
*materialized* into the *scene* at *runtime*. Currently, *actors* require
transform *components* which cause them to be inserted into the *scene tree*,
though this may be relaxed in the future.

- **app**: The application being developed with the engine such as a game, or
visualization, or any other program.

- **appdev**: The person(s) using the engine to develop an *app*.

- **component**: A game object that often looks like a class with slots and
methods and which can be instanced into main memory like classes. However
*components* have additional properties beyond that of classes. One such
property is a global cache based upon the type of the *component*, etc. TBD.

- **concretized metaspace**: A *metaspace* for a production release of the app
made with this engine. All *abstract* pieces of data are intended never to
change (via live coding or any other reason). The *concretized metadata* is a
read-only, optimized, and reduced to required dependencies *metadata* that has
been *realized* to disk. It will be *reified* by the engine at startup time.

- **core** or **core instance**: This is an object in the *runtime* which
represents a running instance of an *app*. While currently there is only
one *core* in the engine, it is intended that there could be multiple
*cores* simultaneously executing on different threads. An example might be
a chess server and two clients--each one represented by a *core* and being
executed concurrently in the same lisp image. Resource are shared as much
as possible between *cores*.

- **DSL**: Domain Specific Language. This is a form or set of forms which are
transformed (often during macro processing) into another form--usually Common
Lisp, and often then compiled. Many *appdev* APIs are in the form of *dsls*.
The engine has a lot of *dsls* both internal and *appdev* facing. A lot of care
goes into constructing useful *dsl* syntax and meaning. It is the data driven
means by which the *appdev* declares much of the behaviour and asset needs of
the *app*. The engine's duty is to bend these data driven specifications into
efficient in memory data structures to reduce resource needs to execute the
*app* and to ensure that *ADAPI* *dsls* slowly change to preserve backwards
compatibility. Please note: An *ADAPI* *dsl* is often used at toplevel in the
*app*'s source code but usually can also be used as a shorthand in the *appdev*
code itself to construct *unregistered* in-memory data structures without
having to call the entire detailed *programmatic API*. It is often true that a
*dsl* expands into its specific *programmatic API*.

    - **Asset DSL: define-asset**: *ADAPI* An asset description language which
    indicates where to find (on disk, or network servers, etc) assets like
    images, audio files, models, custom data, etc.

    - **Config DSL: define-config**: *ADAPI* A configuration language that
    describes *runtime* parameters across a wide spectrum of domains in the
    engine.

    - **Component DSL: define-component**: *ADAPI* A *component* description
    which names it and described the slots and their initialization values.
    TBD

    - **Graph DSL: define-graph**: *ADAPI* A graph dependency description of
    the order of updates for different component types during a frame
    compuation in the engine. The *graph dsl* is intended to have much more
    functionality than it currently does.

    - **Material DSL: define-material**: *ADAPI* A *material* description which
    describes the association between: one or more *textures*, a *shader
    program*, the values (or means by which to compute them) for all the
    required *shader-variables*.

    - **Prefab DSL: define-prefab**: *ADAPI* A *prefab* description which names
    and describes how actors and components (and their initargs) are
    assembled.

    - **Texture DSL: define-texture**: *ADAPI* A *texture* description form
    that names, describes which *texture-maps* are required, and specifies
    the parameters of the *texture object* ultimately created on the GPU.

    - **Texture-map DSL: define-texture-map**: *ADAPI* A *texture-map*
    description form that names, describes the assets required, and specified
    the arrangement of data for a *texture-map*.

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

- **material**: A named association between a *shader program*, a set of
*shader-variable* names along with their values (or how to compute them), and a
set of *textures*.

- **materialization**: Constructing or loading a piece of information (such as
an image, audio file, etc) into the main memory of the app. Examples: loading a
file from disk into an in memory data structure, procedurally generating
information (such as a texture-map image or an entire level) in an in main
memory form, etc. Can be used to represent the construction of a *prefab* into
main memory or the construction of descriptions of data like a *texture object*
from a *texture instance*.

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

- **prefab**: A template which describes one or more *actors* and the
*components* they use along with their coordiante frame, spatial, and/or
referential relationships. A prefab is *materialized* into the *scene* as a
unique set of game objects during *runtime* and it may be *materialized* more
than once to produce independent *runtime* instances of those game objects.
*Prefabs* have a fair amount of control over sharing their innards between
*materialized* *prefabs*. A *prefab* is usually specified by a *prefab dsl*
though it may be constructed using the *prefab* API at *runtime*.

- **programmatic API**: This almost always means a detailed set of lisp
functions and macros specific to the engine which comprise a means to allocate
and initialize various data structures representing such engine concepts as a
*material*, a *texture*, a *texture-map*, etc. The specific API is usually
disambiguated by the context--otherwise we'll say something like *texture-map
programmatic API* to indicate the engine lisp API interface to allocate and
construct a *texture-map* in-memory instance. Note that a *programmatic API*
often includes the fully formed *dsl* that when used not in a toplevel context,
is a shorthand for producing an *unregistered* in memory representation for
something. It is often true that the expansion of a *dsl* expands directly
into its *programmatic API*.

- **realization**: Movement of any data (which has often previously been
*materialized*) to another (non engine) API's managed memory or to another
peripheral's memory. Examples: writing data to audio memory via a device
driver, or passing data ownership/copy off to OpenGL or Vulkan, etc. Additional
examples include: the physical storage disk of the machine (*concretized
metadata might be *realized* to disk), a network server, a virtual file system
in main memory can be a location of *realization*. Data streamed directly from
a source, like disk or network, straight into the peripheral's or API's managed
memory, is still *realized*, just not from a *materialized* source.

- **rectification**: Fill in any missing data in an in-memory data
structure and if there are any constraints in that in-memory data
structure then ensure they are honored as well. A common thing to do
with in-memory data structure from the *ADAPI* DSLs but this term is
general for any kind of in-memory data structure.

- **rectify**: See *rectification*.

- **registration**: This is the process where ownership and control of a data
structure prevously constructed by the *app* is passed from the *app* memory to
the engine.

- **register**: Invocation of an engine API function which does *registration*
of the data structure into the engine.

- **reification**: This is the process (often executed during the engine start)
which will *reify* *abstract* data into *reified* data.

- **reified**: A piece of information is *reified* if it has been transformed
(and often copied) from the *metaspace* into a *runtime*. *Reified* data can be
freely manipulated by the engine and only exists for that unique instance of
that execution. Data may be *reified* from a *concretized metapsace* as well,
which could be read from disk and never transition through the global lisp
environment.

- **reify**: The action of converting *abstract* data in the *metadata* to
*reified* data in the *runtime* at engine start.

- **runtime**: This is all of the code being executed and the ephemeral state
of the engine when the engine is started and executing. When the engine stops
executing, there is no more *runtime*. This word means both the engine and the
*app* in-memory data structure during execution and is disambiguated by stating
the *engine runtime* and the *app runtime*.

- **scene**: The current set of *materialized* *actors* along with their
*components* for which the engine is performing maintenance, updating,
rendering, etc. There is only one *scene*.

- **scene tree**: A directed acyclic graph that denotes the hierarchical
coordinate frame relationships between transform *components* held by *actors*.

- **shader** or **shader program**: A program loaded onto the GPU which defines
and expects values for *shader variables* in order to do its processing in the
shader pipeline.

- **shader-variables**: Uniforms, etc, that name variables to which values are
bound for a specific *shader program*.

- **texture**: An named association between parameter names and values and
*texture-map* data that all should be eventually *realized* into an on gpu
*texture object*.

- **texture instance**: An in main memory object that is part of the *appdev*
API representing that names the parameters of a *texture object* on the GPU and
its association with some *texture-map* data. Often contains a reference to the
*texture object*.

- **texture-map**: A set of (usually) image data (which may include
hierarchical mipmaps, individual image slices of a voxel, or other aggregate
forms) that comprise a 1d, 2d, 3d, or cube-map image.

- **texture object**: Data in the GPU memory representing a texture. It is
constructed, manipulated, and destroyed by the GPU driver API (such as OpenGL,
Vulkan, etc).

- **unregistered**: When an in memory instance is constructed via a
*programmatic API* by the *app*, it only exists in the *app runtime* and this
state is called *unregistered*. It must still be *registered* to the engine so
the engine can locate and resolve all needed assets, validate, and make ready
the information for use by the *app*. An example might be procedural generation
of a *texture-map*. One would construct the *texture-map* with the
*texture-map* *programmatic API* and then *register* it with the appropriate
engine API.
