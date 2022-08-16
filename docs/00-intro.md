# Introduction

`oalex` parses and extracts text. It can either process text directly from the
command line using standard input, or by generating C++ parser code. It stands
out in the legibility of its syntax for parsing rules (aka. the input grammar).
If you have notes or sample text for what your input text will look like, you've
already done most of the work. Very little cognitive effort is required to turn
what you have into the rules you need for parsing.

This also helps if you need to return to your code months later, and are trying
to remember where you left off. You'll find the parsing rules to be choke-full
of reminders and hints, in the syntax that made sense to you at the time you
wrote it. This reduces the layers of reverse-engineering you have to do to get
back to work. Even better, `oalex` natively supports unit tests, so you will
immediately know what input you had intended to parse, and what input you meant
to reject.

I wrote `oalex` when I was getting tired of scrutinizing a pile of `split()` and
`trim()` calls five months after I wrote them. Even with regex or parser
generators, the task of looking at a grammar and then visualizing what syntax it
stands for isn't always easy. Hopefully this tool helps.

The [first lesson](01-pattern-basics.md) starts with a very simple parser for an
`a + b` expression. In the later lessons, we will build up on that to parse more
complicated inputs. Eventually, we should be able to apply this to real-life
inputs like parsing random log files or other random syntax.

Planned table of contents, subject to change:

  1. [Pattern basics](01-pattern-basics.md)
  2. Optional components and repeats
  3. Composing multiple rules together, and single-line rules
  4. Alternate choices and error-handling
  5. Unit tests, and how it improves readability
  6. Comment-processing, space-handling, and newlines
