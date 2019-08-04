* 2 Aug 2019. Reduce `dfa_test.cpp` boilerplate.
* 2 Aug 2019. Error and warning propagation.
* 2 Aug 2019. Add zero-width anchors like '\b', '^', and '$'.
  - GlrCtx now holds prevCharType as enum.
  - GlrCtx::shift() can check current node type and decide between
    shifting terminal string or shifting anchors.
  - checkError can enforce node types:
    * Nodes with anchor edges going out must have exactly two edges: one
      for the anchor and one without.
    * Nodes without anchor edges are just like nodes today.
