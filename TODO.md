* 2 Aug 2019. Reduce `dfa_test.cpp` boilerplate.
  - Includes the GssHooks API changes below.
* 2 Aug 2019. Error and warning propagation.
* 2 Aug 2019. Add zero-width anchors like '\b', '^', and '$'.
  - GlrCtx now holds prevCharType as enum.
  - GlrCtx::shift() can check current node type and decide between
    shifting terminal string or shifting anchors.
  - checkError can enforce node types:
    * Nodes with anchor edges going out must have exactly two edges: one
      for the anchor and one without.
    * Nodes without anchor edges are just like nodes today.


De-emphasizing `extend()`
-------------------------

2nd Aug 2019

Right now `class GssHooks` looks like this:

  * `extend()`: called on every non-PushEdge
  * `useValue()`: called on every PushEdge
  * `merge()`: works on SemVal

For the most part, they directly work on `shared_value`s. The typical
implementation of these methods, however, are too messy. Typically, extend can
always form a cons list. Most rules are not ambiguous, so `merge()` becomes
unnecessary. I would rather make it work like this:

`class GssHooks`:

  * `merge()` Only works on ListVal. By default errors out.
  * `reduceList()` Gathers up ListVal into custom values. More like
    `useValue()`, but it doesn't matter if return LabelEdge goes through a
    PushEdge. `SharedVal reduceList(DfaLabel,ListVal)`.
  * `reduceString()`.

The old API can still be available as `GssAggregator`. `extend()` calls are
automatically aggregated into `ListVal`s. This old API will only be used for
implementing the new one, and will not be used in `dfa_test.cpp`. I might even
make it internal for now.


3rd Aug 2019

Plan:
  1. Rename GssHooks to GssAggregator. (done)
  2. Implement ListVal. (done)
     - Has utilities for size and indexing. (done)
     - `dfa_test.cpp` can optionally have structured dynamic_cast bindings.
  3. Implement new GssHooks to use ListVal and expose the new API above. (done)
  4. See if we can simplify `dfa_test.cpp`


Cleaning up `dfa_test.cpp`
--------------------------

2nd Aug 2019

We will use this cleanup as a proxy for general API cleanup.

  * `useValue := return val` is pretty common
    - When it's not, we try to match up DfaLabel with `shared_value`.
  * Default BugMe seems reasonable, at least in test.
  * `dynamic_cast<const StringVal*>(foo.get())` is really common.
  * Handle some state and edge in `extend()`, error out of the rest.
  * Use `combineValue<...>()` to encourage proper stPos and enPos.
  * `BugMe<<"called unexpectedly"` should become return unexpected.
    Have this propagate out somehow.
  * `extend()` API doesn't quite support if clauses on edges. I'm checking
    fromState manually.
    - At the end of the day, `extend()` is only needed in the face of merge().
      It is unnecessary if we don't care about `merge()`. And if all merges
      ever deal with are a list of SemVals, we might not need `extend()` at all.
      Cleans up Hook code quite a bit. Some labels might need to ignore things.
    - This might in fact be the default. But the current extend/merge might
      still be useful for unit tests.

What would be a better error message in real (i.e. non-test) usage?
Can't really do much better unless I know state and label semantic meanings.

Now there's an idea: DFAs could have named versions just for error message.

ConcatListVal has an interesting Hook. They need to make sure an edge
corresponds to a SemVal state.
