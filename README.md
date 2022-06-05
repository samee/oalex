`oalex`
-------

`oalex` is a parser generator for rapidly prototyping new language syntax.
For example, this could be defining the syntax for your own custom language:

    rule my_function_declaration:

      fn name(type param, type param, ... , type param)

    # Identify various parts of the pattern above.
    where:
      name, type, param ~ /[a-zA-Z_]+/

The grammar above can now be saved in a file (say, `example.oalex`) and be used
to parse an example input as follows:

    $ echo 'fn connect(int x, string s)' | oalex eval example.oalex
    {
      "name": "connect",
      "param": [
        "x",
        "s"
      ],
      "type": [
        "int",
        "string"
      ]
    }

You can keep tweaking the syntax until you are happy with it, and then generate
the parser code:

    $ oalex build example.oalex
    $ ls example.*
    example.cpp  example.h  example.oalex

A copy of this example can be found in `testdata/readme-example.oalex`.

This project is still very much a work-in-progress, but you are welcome to play
around. The files in `testdata/*good*.oalex` will give you a good idea of its
current capabilities at any given time. You'll notice that it already supports
in-file unit-tests for your grammar, with an `oalex test mygrammar.oalex`
subcommand.

If you want to try it out, just clone this repository and use the typical
`cmake` steps:

    cmake .
    make
    ./oalex --help

Just a little disclaimer: this is my own project that I work on in my personal
time, and is not officially supported by my employer.
