Done fixing bug in map-output branch!

Currently rebasing. Need to:

  * change type of createOptionalRule(), declaring the bug fixed.
  * Continue propagating the rule type to top-level callers (e.g.
    compileSingleExpr), and test those changes.

Stashed on 29th May.

Found the bug again! Double-quoted "hello [world]" without any fields coming
out of the optional thing. I always wanted the compilation process to produce
a map/string status.

Adding output type to compilation result
----------------------------------------

For now, the outputs are threefold: `string`, `flat_map`, and `frozen_map`.

PatternToRulesCompiler. Outputs are fairly straightforward. Only a few produce
literal, everything else makes a flattenable map. They map directly from the
dynamic type of the pattern object.

processDquoted uses the PatternToRulesCompiler directly.

Steps to fixing bug
-------------------

  * Reuse CompiledSingleExpr as the return type of compileRuleExpr(). [done]
  * Add output type as a field to CompiledSingleExpr (and CompiledRuleBranch).
  * Use the output type to decide whether additional wrapping is needed when
    using OutputTmpl.
