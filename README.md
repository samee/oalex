# Overly ambitious lexer (oalex)

I often want a quick-and-dirty parser for tiny custom languages, but existing
parser generators often required me to put in too much thought. Manually
written parsers, on the other hand, were often not robust enough since the
verbosity always tempted me to cut corners. This was meant to be a lightweight
lexer that takes care of the tedious common cases, but left the meat of the
logic to an application. The initial designs, however, quickly acquired things
beyond the regular grammar a lexer is supposed to handle.

At this point, there is no code, and this is where I will put free-form design
docs. Eventually, if I feel like it, it will turn into actual code that does
something useful.

Disclaimer: this is just something I work on in my own free time, and is not an
official Google product.
