# reconstructorepl

An in-progress experiment in making it trivial to move a complete, minimal tree of interdependent expressions from the REPL to a source file.

## Usage

I do a lot of my coding in the REPL. Often after an extended session, in which I've tried various approaches to a problem, including various missteps, I've found a solution I like. Then I have to go back through my REPL history and try to pick out only the complete, minimal sequence of expressions that compose my solution. I may have redefined the same var multiple times as I refined my understanding, and have to be sure to pick out only the correct version. Often I've cleared the buffer, which means I can't jump to the definition. It's always doable -- but after a long REPL session, it can be tedious and error-prone.

Reconstruct-o-REPL is an experiment in making that process trivially easy.


...

Two strategies for causing defining forms to be stored: a custom REPL or custom def/defn.

Two places to store the forms: a central registry or var metadata.

0) For both strategies, start in an ordinary REPL.

1) A REPL with a custom :read function (`reconstructorepl.core`). You can start the custom REPL via `(saving-repl)`. While (or after) using it, calling `(build-defs 'a)` (for some var `a` defined in the custom REPL) will print a complete, minimal, ordered sequence of statements which can be copy/pasted into your source file.

2) (`reconstructorepl.redef`): within the ordinary REPL, just call `def'` and `defn'` in place of ordinary def and defn. At any time, call `(build-defs 'a)` as before.



## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
