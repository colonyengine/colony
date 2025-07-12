# DSL construction rules

The engine presents a highly declarative API via DSLs to the appdev which
allows the appdev to describe assets and many other things.

It is often the case that a feature for the engine (and often different/new
asset modalities) will require a new DSL (and accompanying programmatic API).
Unless there is a specific reason, the same programmatic API to the DSL's
in-memory representation is used both by the engine and also exported so the
appdev can use it too.

Of course, these recomendations are guidelines. Some DSLs can be so simple or
perform a clear feature that doesn't require the work below. The real hint to
figuring out how much work you need to do is by asking yourself this question:
"Will the appdev have to manipulate the in memory object constructed by the DSL
at runtime, or be able to make those in memory objects at runtime?" If the
answer is yes, then this scaffolding is most likely useful to follow.

Here is the general ordered way to think about and implement the DSL and the
feature that goes with it. These are high level steps and each step may require
more or less effort depending on the contextand meaning of the DSL--some steps
might beremovable too! Usually MOST of the work for each step is done and then
afterwards, iterative development happens across the entire new system to
complete the work.

1. Familiarize yourself with the conventional features most of our DSL have,
like attribute specification for use with an attribute-bag, the general syntax
which favors manipulation slightly more than readability, etc. We have some
codes that do some of this work already and you should use them.  We want
homogeneity in our DSL when we're able to do it.  TODO: Describe these
conventional features in more text later in this document.

2. Design the DSL to represent all the required cases. The DSL might split out
into a Logical Form and a Physical Form--or there might only be a Physical
Form. If there are two forms, they both must be acceptable input.

3. Design all the programmatic API data structures and the Interface API to
make them and manipulate them. Design at least the data structures and
constructor methods/functions. This is the API that will be used by the engine
and the appdevs in their app. Code these data structures and programmatic API.

4. Write the macro expander that goes from logical form -> physical form ->
programmatic API. If the DSL is intended to be extensible, ensure to carefully
design the transformer code to be extensible. You may not get this right the
first time. This is fine, you can get it working the best you can and evolve it
later. Depending on the needs of the DSL, the generated programmatic API might
be in a lambda thunk to delay its evaluation. 

5. The macroexpander may have to understand the difference between: expanding
at the toplevel or expanding at non-toplevel, expanding when the engine is
running (as in live coding) or expanding just during loading of the codebase.
There are ways to make these observations.

6. Add your DSL and the programmatic API to the test suite to ensure that
everything works like it should and stays working.

Here is the general dataflow during the compile/runtime phase for the data
described by the DSL.

At compile/macro-expansion time:

- Logical DSL form (if present) macro expands to a ->
- Physical DSL form, which is normalized, and continues to expand into the ->
- Programmatic API form which often is thunked and ->
- Stored in the metaspace (OR simply used in appdev code).

Then at runtime after the engine starts:

- The thing the DSL represented is wanted to be used ->
- The thunk is forced ->
- The in-memory data structure is reified/registered to the engine then ->
    - Materialized ->
      - data-elements are brought into main memory in the resource-cache ->
      - rectification-classification ->
      - Rectified ->
        - synthesis ->
        - validation ->
- At this point the data is in main memory and ready to be used. It may need ->
    - Realization, to get it to the GPU, audio card, third party API, etc.

If the DSL was used as a handy means to build an instance of the in-memory data
structure in the appdev code, the in-memory data structure usually must be
registered with the engine, after which it usually goes through the runtime
procedure detailed above.

For backward compatibility reasons, the DSL should be designed to be relatively
future proof. The in-memory representation's type system should partition out,
when/if appropriate, data specific to the DSL's concept versus data specific to
is relationship to the engine. This is to minimze the impact to appdevs if the
relationship of the in-memory data structure radically changes wrt the engine.
This is a subtle concept and might or might not be useful depending on the DSL
and its use, so please ask.
