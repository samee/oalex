* 24 Aug 2019. Add error reporting support.
* 13 Aug 2019. GssAggregator can probably be removed now.
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
* 5 Aug 2019. Every DFA seem to need this weird single-edge LabelEdge at the end
  to force useVal. Shouldn't be necessary.


Error reporting support
-----------------------

We'll support two kinds of diagnostics: recoverable and non-recoverable.

Recoverable diags can be either errors, warnings, or info. We won't make any
distinction at the Dfa level. They get aggregated into GssHead/GssEdge using
`shared_ptr` of `Diag` or `DiagList`, automatically on all non-negated
reductions (right now we don't support negations anyway). We will provide some
way of collecting and surfacing these errors out of a given parse tree at the
end.

Non-recoverable errors can be either:

  * Unexpected characters.
    - Input: "bard" raising "unexpected 'bar', expecting 'bazaar'". A little
      weird because of the missing 'd', can be customized later when we have
      negations.
  * Heads that never reach enLabel.
  * Reductions explicitly abandoned.

Since multiple heads are possible, we report errors from the last non-negated
reduction only for now.

The API we provide shouldn't require users of glrParse to check for
non-recoverables separately, at least for the common case of unique parsing
tree.

  A note about `nullptr`. Today it means AbandonedReduction. This means we
  cannot attach any diagnostic message to it. Moreover, EmptyVal now means
  something different from nullptr. Let's fix this craziness while we can. We
  will change current uses of nullptr to use AbandonReductions, and then define
  nullptr to be the same as EmptyVal.

  16th Sept 2019: I am having second thoughts about the above. EmptyVal can
  signify successful parsing without information. nullptr today causes us to
  abandon parsing. If I want abandonment to be explicit, I should add explicit
  `bool abandon` to GssHooksRes. But then we need to think about how nullptr
  SharedVal propagates.

  The whole thing that started this thought process is how to record lastDiags
  in GlrCtx::parse. For that we need changeHead and extendHead to report diags
  somehow. The question is do we make a variant for that, or do we reuse
  GssHooksRes? For that, we ended up revisiting how abandonment is indicated.

Plan:

  1. Add support for DiagList propagation and gathering.
  2. Support explicit AbandonedReduction and gathering.
     - Need to add Diags on unexpected input characters.
  3. Support unambiguous root convenience gathering.
  4. Wire unexpectedChar and noEnLabel to AbandonedReduction.
  5. Change current uses of nullptr to use AbandonedReduction.
  6. Change the meaning of nullptr to mean EmptyVal.


Cleaning up `dfa_test.cpp`
--------------------------

2nd Aug 2019

We will use this cleanup as a proxy for general API cleanup.

  * `useValue := return val` is pretty common
    - When it's not, we try to match up DfaLabel with `shared_value`.
  * Default BugMe seems reasonable, at least in test.
  * `dynamic_cast<const StringVal*>(foo.get())` is really common.
  * Use `combineValue<...>()` to encourage proper stPos and enPos.
  * `BugMe<<"called unexpectedly"` should become return unexpected.
    Have this propagate out somehow.

What would be a better error message in real (i.e. non-test) usage?
Can't really do much better unless I know state and label semantic meanings.

Now there's an idea: DFAs could have named versions just for error message.

ConcatListVal has an interesting Hook. They need to make sure an edge
corresponds to a SemVal state.
