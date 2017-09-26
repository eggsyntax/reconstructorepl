# reconstructorepl

An in-progress experiment in making it trivial to move a complete, minimal tree of interdependent expressions from the REPL to a source file.

## Usage

I do a lot of my coding in the REPL. Often after an extended session, in which I've tried various approaches to a problem, including various missteps, I've found a solution I like. Then I have to go back through my REPL history and try to pick out only the complete, minimal sequence of expressions that compose my solution. I may have redefined the same var multiple times as I refined my understanding, and have to be sure to pick out only the correct version. Often I've cleared the buffer, which means I can't jump to the definition. It's always doable -- but after a long REPL session, it can be tedious and error-prone.

Reconstruct-o-REPL is an experiment in making that process trivially easy.

...

This approach can vary on (at least) two axes. First, there are two strategies I've come up with for causing defining forms to be stored: working in a custom REPL or using a custom variant of def/defn. Second, there are two places where the extra info to be stored: in a central registry or in var metadata.

Here are the two strategies I've implemented so far:

0) For both strategies, start in an ordinary REPL.

1) A REPL with a custom :read function (`reconstructorepl.core`), which stores the form in a registry. You can start the custom REPL via `(saving-repl)`. While (or after) using it, calling `(build-defs 'a)` (for some var `a` defined in the custom REPL) will print a complete, minimal, ordered sequence of statements which can be copy/pasted into your source file.

2) Custom def and defn, storing the form in the var metadata: within the ordinary REPL, just call `reconstructorepl.redef/def'` and `reconstructorepl.redef/defn'` in place of ordinary def and defn. At any time, call `(build-defs 'a)` as before.



## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
