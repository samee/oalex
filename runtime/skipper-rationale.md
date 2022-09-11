skipper.h
=========

`class Skipper` is a component in what I hope will become a parser generator.
This document describes why this class was designed the way it was, just so I
remember my own thought process.

Objects of this class simply start at a given position, then skip over
whitespace and comments for the next character in a stream. That next character
is usually the start of the next token. It's a glorified `std::string::find()`
that takes in a list of comment delimiters. This search can be optionally
constrained to a single line. This small utility turns out to be fairly reusable
across parsers of a wide variety of formal languages.

My thought process started with generating lexing and parsing for each language
like textbooks would have us do. I then took account of how such a parsing
architecture conflicts with many popular languages in use today. Finally, I used
the list of such conflicts as design constraints for how I want to structure
my parsing code.

In the end, it looks like I may not need to generate code for a separate lexer
phase at all.


What's a lexer?
---------------

[Wiki link](https://en.wikipedia.org/wiki/Lexical_analysis).

In textbooks, a lexer (also known as scanner or tokenizer) turns a stream of
characters into a stream of tokens. `int x = 5;` becomes `["int", "x", "=",
"5", ";"]`. It's the first stage, before the context-free parser. In the
[Chomsky hierarchy](https://en.wikipedia.org/wiki/Chomsky_hierarchy), a lexer
mostly deals with a regular language, while context-free aspects remain for a
later stage. These later stages will ideally not need to deal with raw
characters, but whole tokens.

For many programming languages like C or Java, lexing filters out spaces and
comments. It tags outputs tokens as operators, identifiers, quoted strings, etc.
The later parsing stages then have fewer details to worry about.


An awkward fit
--------------

Unsurprisingly, the human sense of aesthetics do not always align well with the
rigid distinction between the lexing and parsing phases of formal language
processing. Here, we list several well-known examples of this misalignment. We
later use these examples as design constraints for an amended architecture.

**String expansions**. Bash, PHP, and many other languages allow arbitrary
expressions in strings, like `"$(echo "hello" world)"`. Like it or not, it's a
common feature of many scripting languages. There is no concept of a "string
literal token" that the lexer can return here without the help of the full
parser. We cannot simply start at one quote symbol and end on the next.
Even to just find the matching end-quote,
it needs to invoke the parser with a full symbol
table of variables, and an extra instruction that it should end parsing on the
next unmatched `)` character. It is just less useful to think of lexing as
a separate phase here.

**HTML and Javascript**. Since one can embed one language inside another we
absolutely have to allow switching the input stream between two different
parsers. There can be no single tokenizer object in our code that abstracts
away the stream of characters. Instead, tokenizers need to report how much they
have consumed and what mode the parser should continue in for the next token.

**Indentation**. Python famously uses indentation to indicate grouping without
braces. So does Haskell, YAML, and many others. If it didn't, spaces outside
string literals could be consumed by the lexer and not passed on to the later
stages of parsing. But now it needs to pass on special `INDENT` and `DEDENT`
tokens. While very elegant, the tokenizer still needs to switch on or off its
indentation-sensitivity depending on whether it's inside brackets or outside them.
This forces code duplication. Example:

    $ python -m tokenize <(echo "
    if True:
        x = [5,
    2]")
    1,0-1,1:        NL      '\n'
    2,0-2,2:        NAME    'if'
    2,3-2,7:        NAME    'True'
    2,7-2,8:        OP      ':'
    2,8-2,9:        NEWLINE '\n'
    3,0-3,2:        INDENT  '  '
    3,2-3,3:        NAME    'x'
    3,4-3,5:        OP      '='
    3,6-3,7:        OP      '['
    3,7-3,8:        NUMBER  '5'
    3,8-3,9:        OP      ','
    3,9-3,10:       NL      '\n'
    4,0-4,1:        NUMBER  '2'
    4,1-4,2:        OP      ']'
    4,2-4,3:        NEWLINE '\n'
    5,0-5,0:        DEDENT  ''
    5,0-5,0:        ENDMARKER       ''

While it produces the expected `INDENT`, notice that it does not produce a
matching `DEDENT` token right before `NUMBER '2'`. Python developers had to
perform bracket-matching once in the tokenizer, and again in the later parser.
It should be possible to rearchitect things with bracket-matching done only
once.

**C++ template angle-brackets**. Way back in C++98, template types used to
require an extra space between closing angle brackets:

    vector<vector<int> > v;
    // vs.
    vector<vector<int>> v;

This was because the tokenizer had already parsed `>>` as the left-shift
operator, which could not be matched with two `<` signs anymore. While it was
[solved in C++11](https://en.wikipedia.org/wiki/C%2B%2B11#Right_angle_bracket),
the solution still requires deconstructing an already-formed `>>` token
back into two angle brackets at later stage of parsing. This
again raises the question of whether we have the correct abstraction boundary.

The above demonstrates that we are already deviating from the classical parsing
pipeline in multiple, wildly successful languages. It seems likely that new
languages of this sort will keep getting invented in future. It makes sense to
see if we can rearchitect the canonical parser to accommodate these demands
instead of requiring the world of future language designers to change their
taste.


A few more quirks
-----------------

While we are at it, it may well be useful to look at a few other interesting
features of these common languages that might also throw a wrench into our
works. While they are not quite as painful, they are certainly an awkward fit.

**Nested comments**. It's a minor issue, but some languages support nested
comments, which are not technically "regular". OCaml uses `(* *)` pairs.
Haskell uses `{- -}`. Even C has `#if` and `#endif` directives that are treated
like comments by compilers
([Clang](https://clang.llvm.org/docs/InternalsManual.html#the-lexer-and-preprocessor-library))
and editors (Vim) alike. They are not regular languages. To be fair, real-life
real-life regex engines have supported recursive patterns for a while. Lexer
genreators like `flex` also support nested comments ([StackOverflow
thread](https://stackoverflow.com/questions/34493467/)). It still raises
the question of if we should keep the lexer a separate phase at all.

**Raw strings** or **heredocs**. C++ has [raw literals](https://en.cppreference.com/w/cpp/language/string_literal),
Bash and PHP has
[heredocs](https://linux.die.net/man/1/bash).  Similar to above, they are not regular languages. In both cases, it
allows the user to specify an arbitrary end-marker for a string literal, so that
quotes, backslashes, and other special characters can be used in the string body
without fear of being interpreted. As a result, lexers need special features to
tokenize these languages.

**Paragraph breaks**. LaTeX and markdown both interpret blank lines as paragraph
breaks. Their parsers are generally happy to process a stream of tokens not
caring about whitespaces, except when blank lines show up. In that case they
want to collapse consecutive blank lines. LaTeX specially has very strange rules
around whether lines with comments count as paragraph breaks (they don't), and
whether blanks around a comment can get collapsed (they can). `verbatim` blocks
obviously don't count. Markdown frequently allows embedded html, which can also
complicate things.


Design points
-------------

  * Even simple tasks like finding the end of a token can be very
    language-specific.
  * We want to easily switch an input stream between parsing contexts.
  * We should see whether we can implement common primitives that can make life
    easier when writing parsers, without taking in too many language-specific
    quirks.
  * This may require a different parsing architecture.

We ended up with a combination of two abstractions:

  * `class Skipper` that only finds the beginning of the next token,
    not the end. It provides a few features for paragraph breaks too.
    - For very simple cases, we will probably provide separate utilities for
      finding the end of tokens as well. E.g. end of an identifier based on a
      customizable set of "word chars".
    - For more complex cases finding the end is better left to language-specific
      logic.
    - It's stateless beyond some initial configuration, to help switch between
      skipper objects mid-parsing. Like `std::string::find()`, it doesn't
      remember anything about which input stream it was last working on.
  * `class Input` that keeps a buffer of our working window, allowing us to
    mostly treat the streaming input as if it's all in memory.
    - Looking ahead to a later address causes the buffer to expand with more
      input data.
    - The buffer can be shrunk back with an explicit `forgetBefore(pos)` call.
    - It has an upper limit on this buffer's size.
    - It keeps track of all newlines in a stream to help convert byte index to
      and from row-column positions.

I toyed with the idea of handling common escape codes like `\n`, `\t`. But with
hex, unicode, line continuation, string substitution, it all turned out to be
too language-specific. We may revisit this choice later.


Expected coding patterns
------------------------

Consider a language with this simple rule in its grammar:

    plus_expr ::= expr "+" expr

Let's also assume that it allows Haskell-style comments. The parser would look
like this:

    // Lists comment delimiters, taken from Haskell.
    Skipper skip{{{"--","\n"}}, {{"{-","-}"}}};

    // Use skip.acrossLines() to indicate we don't care about newlines.
    // Use it before every term in the grammar above.
    optional<PlusExpr> parsePlusExpr(const InputPiece& input, size_t pos) {

      pos = skip.acrossLines(input, pos);
      optional<Expr> expr1 = parseExpr(input, pos);  // expr
      if(!expr1) return nullopt;
      pos = expr1->end_pos;

      pos = skip.acrossLines(input, pos);
      optional<Terminal> terminal = parseTerminal("+", pos);  // "+"
      if(!terminal) return nullopt;
      pos = terminal->end_pos;

      pos = skip.acrossLines(input, pos);
      optional<Expr> expr2 = parseExpr(input, pos);  // expr
      if(!expr2) return nullopt;
      pos = expr2->end_pos;

      PlusExpr result;
      result.expr1 = *expr1;
      result.expr2 = *expr2;
      result.end_pos = pos;
      return result;
    }

It's fair to say that the translation from the grammar to the parser is
extremely mechanical, yet amenable to human debugging. This already allows
everything necessary for spaces and comments to appear at appropriate points,
all without needing a full `flex` input file. The "tokenization" is hidden
inside various `parseTerminal()` calls. The parser's full context is available
here, so we will never have to "break" tokens from `>>` to `> >`. It should be
noted that real code must also provide appropriate error messages for each early
returns on failure. Here we assume they are provided by the subprocedure calls.

It is also evident how we would handle changing skippers between various
contexts. The `Skipper` object does not remember any `input` or `pos` from one
`skip.acrossLines()` call to another.  So in a Python-like language, for
instance, we can switch over between `Newlines::ignore_blank` and
`Newlines::ignore_all` depending on whether we are in a bracketed context or
not. Or even switch to a different `Skipper` object altogether if we are
switching into parsing Javascript inside HTML.

As for indentation-tracking, `Input` already keeps track of all newlines, so
indentation of any token can be computed by examining the sequence of tabs and
spaces between its starting position and the preceding newline.

We provide an extra boolean flag `skip.acrossBlankLines()` to indicate if
`skip.acrossLines()` should stop at blank lines. If it does, `skip.blankLines()`
is a utility to find the end of the blank block, which the parser can analyze.

Pretty much all our complexity from having a separate lexing phase disappears
since we can now handle them in the main parser.


Relation to scannerless parsers
-------------------------------

Every scannerless parser
([wiki list](https://en.wikipedia.org/wiki/Scannerless_parsing)), including this
one, needs special handling for whitespace and comments. This document describes
one such approach. In particular we are interested in the internal code
organization necessary for supporting such special cases, with the minimum set
of simple features necessary to support a wide variety of language constructs.
