# Lesson 1: Running `oalex` and generating outputs

First, we'll start with the staple of any parser tutorial, parsing arithmetic
expressions! We'll start with the simplest of them all—a sum of two numbers of
the form `1 + 2`. In this lesson, we will see how to parse these expressions
with `oalex`, customize its output, and embed parsers into other programs. In
later lessons, we will build up to general, recursive arithmetic expressions.

Let's open a new file named `simple_sum.oalex`
([link](sample_code/simple_sum.oalex)) and write the following grammar:

    rule simple_sum:
      a + b
    where:
      a, b ~ /[0-9]+/

This is how we define a new syntax in `oalex`. In plain English, this grammar
means:

    Let's define "simple_sum" as an expression with the following syntax:

      a + b

    Where,

      a, b  represents any text that matches the regex pattern /[0-9]+/,
            i.e., a string of digits.

If you are not familiar with regex patterns, you can search the web for some
excellent regex tutorials (e.g. http://regextutorials.com/). The plus sign ("+")
in `a + b` is not a placeholder for anything. It indicates that we will
literally match a "+" sign in the input.

You can now save the rule above into a text file named `simple_sum.oalex` and
use it for parsing:

    # Operands parsed out as expected
    echo '34 + 23' | oalex simple_sum.oalex
    { "a": "34", "b": "23" }

    # Spaces between tokens are not significant by default
    echo '34+23' | oalex simple_sum.oalex
    { "a": "34", "b": "23" }

    # Semi-sensible error messages are provided by default
    echo 'x+y' | oalex simple_sum.oalex
    error: Does not match the expected pattern

A copy of the `simple_sum.oalex` file, as well as all other examples in this
tutorial, is available in the `docs/sample_code` directory.


## Syntax components

Let's examine the rule above once more, and walk through various parts of the
syntax:

    rule simple_sum:
      a + b
    where:
      a, b ~ /[0-9]+/

Here, the keyword `rule` indicates that we are defining a new parsing rule. This
is followed by the name of a rule, `simple_sum`. Every rule in `oalex` must have
a name. This name is used to refer to the rule when it is a sub-component in a
different rule. The name is also used to generate the name of the parser
function produced by the `oalex build` command that we will see later.

The next line `a + b` is the pattern. This part is indented a little to the
right just to set it apart, and is not processed by `oalex` just yet. From just
the first two lines, `oalex` has no way of knowing which part of this pattern is
fixed, and which parts are standing in as a placeholder for other text. That's
what we do in the `where` stanza.

The `where` stanza in this case says that `a` and `b` in the pattern _matches_
the regex `/[0-9]+/`. Throughout `oalex`, the operator `~` is read as "matches".
Let's say we wanted to accept operators other than just `+`. That change is simple:

    rule bin_expr:
      a op b
    where:
      a, b ~ /[0-9]+/
      op ~ /[-+*\/]/

Here we have replaced the plus sign in the pattern with `op`, which is now
defined to match another regex that accepts all four arithmetic operators.

Testing it out:

    echo '2*3' | oalex bin_expr.oalex
    {
      "a": "2",
      "b": "3",
      "op": "*"
    }

However, this obscures the input syntax a bit. As a reader, it takes a bit of
back-and-forth between the pattern and the `where` stanza for me to visualize
what the input is supposed to look like. In order to make the rule definition a
bit more readable, you can also use a _quoted placeholder_ for the pattern:

    rule bin_expr:
      a + b
    where:
      a, b ~ /[0-9]+/
      "+" as op ~ /[-+*\/]/

Here we went back to using `+` in the pattern rather than the word `op`, but
later redefined it as a placeholder. A quoted placeholder allows parts of the
pattern to become placeholders even if they don't look like a valid name. This
still parses the exact same syntax, and produces the exact same output. However,
it makes the intended syntax more immediately obvious to the human reader. 

> An aside on the regex: In the pattern just added, the minus sign (`-`) is the 
> first character so that it is not indicating a range. The division operator 
> (`/`) is also escaped with a backslash (`\`) so it is not counted as the end
> of the regex.


## Output templates

There is one last thing we want to change in our `a + b` parser: the output
formatting. When you run the `simple_sum` rule above, you see an output like
this:

    # Operands parsed out as expected
    echo '34 + 23' | oalex simple_sum.oalex
    { "a": "34", "b": "23" }

The actual output might be broken up into multiple lines, but let's ignore
output whitespace for now. The names `a` and `b` only make sense in the context
of the rule definition. They are not the most descriptive in the output. So you
can change how the result is presented by using an _output template_.

    rule simple_sum:
      a + b
    where:
      a, b ~ /[0-9]+/
    outputs: {
      left: a, right: b,
      operator: "+"
    }

This new `outputs:` stanza tells `oalex` how we want the output to be
presented. If this is not provided, `oalex` will try to synthesize a reasonable
default template on its own. But in this case, we will get better names in the
output. Quoted strings (like the `"+"`) are passed on to the output unchanged,
while unquoted names (like `a`, `b`) are replaced by the corresponding input
part that they represent. If we once again save this rule with the changes
above, we can see the new output:

    echo '34 + 23' | oalex simple_sum.oalex
    {
      "left": "34",
      "operator": "+",
      "right": "23"
    }

It still parses the same input. If you are wondering about the ordering of the
three fields, they are always sorted alphabetically. This is not a problem,
since the output is meant to be consumed by a program, and any conforming json
parser will disregard the relative ordering. Later versions of `oalex` might
become better at maintaining the specified format for better human readability.


## Compiling into C++

While we have been using `oalex simple_sum.oalex` to quickly evaluate and test
our grammar, if you run `oalex help`, you will see other `oalex` capabilities:

    $ oalex help
    Usage:  oalex [eval] syntax_file.oalex           # Parses stdin
            ...
            oalex [eval] test syntax_file.oalex      # Runs all examples
            oalex build [--cpp-out=outname.cpp] [--h-out=outname.h] \
                  syntax_file.oalex                  # Generate all parsers
            ...

As you can see from the first line, `oalex simple_sum.oalex` was really a
shortcut for `oalex eval simple_sum.oalex`. `eval` is the default command. The
`eval test` command is for running unit tests, which we will come back to later.
Now we will look at the `build` command, which allows us use `oalex`'s
capabilities from inside another C++ program. In future, `oalex` may support
other language backends too.

Let's try this out:

    $ oalex build simple_sum.oalex
    $ ls simple_sum.*
    simple_sum.cpp  simple_sum.h  simple_sum.oalex

Two new files have been created, a `.cpp` file and a `.h` file. You will notice
that the generated `.h` file now declares a parsing function named
`parseSimpleSum()`, among many others. This name was generated using from the
name provided to the `rule` keyword above. To test this, make a new file with
the [following contents](sample_code/simple_sum_test.cpp) and save it as
`simple_sum_test.cpp`:

    #include "simple_sum.h"
    #include <iostream>

    int main() {
      oalex::InputDiags ctx{oalex::Input{"1+2"}};
      ssize_t pos = 0;
      oalex::JsonLoc jsloc = parseSimpleSum(ctx, pos);
      std::cout << jsloc.prettyPrint(0) << std::endl;
    }

This program will parse the string `1+2`. To build and actually run the test
program:

    $ export SRC=/path/to/oalex/src
    $ export BUILD=$SRC/build
    $ cd $SRC/docs/sample_code
    $ g++ simple_sum.cpp simple_sum_test.cpp \
          -L $BUILD/runtime -l:liboalex.a -I $SRC/runtime \
          -L $BUILD/submodules/fmt -l:libfmt.a
    $ ./a.out
    {
      a: "1",
      b: "2"
    }

This produces the same output as the first example we saw at the start of the
lesson, parsing the input in the same way.


# Conclusion

This concludes lesson 1. You can now write simple rules, test it out, format the
output json objects, and use the result in your own C++ programs. The next
lesson will cover a bit more complex rules, where we have optional or repeating
parts. We will also see how a file can have multiple rules, one using the other.

I'm hoping to post the next lesson in about 2-3 weeks.
