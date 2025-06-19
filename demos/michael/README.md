## Functions as relations

`appendo.rkt`, `regex.rkt`, `quasiquine.rkt`, `proofo.rkt` all show pretty much the same thing: we can implement a relation by writing a function and running it in the interpreter. In all but `appendo.rkt`, the function is an interpreter for another language, so we get a synthesizer for that language.

Start with `appendo.rkt` to show that we're generating miniKanren code similar to hand-writing the relation.

## Synthesis with ground context

`synth-append.rkt` and `synth-fib.rkt`; the latter shows a larger ground context in the form of a library of helper functions.


## A small interpreter

`or-evalo.rkt`.