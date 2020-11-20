# Staged miniKanren

Staged miniKanren is an extension of miniKanren that supports staging. In particular, we apply partial evaluation techniques to relational programming. 

(update: staged programming is manual partial evaluation, uses manual binding time analysis. Manually decide to lift unifications. Manually decide to Scala use type checking of Scala to help us with bind time analysis. Staging is partial evaluation with offline manual binding time analysis. The whole point of staging is you have control of binding time analysis; this is more powerful. With dynamic -- annotate some variables by hand, and the rest will follow.)


## What is partial evaluation
Given a program and only partially known input data, partial evaluation allows one to eliminate or simplify certain parts of the code, by for example, executing the parts that don’t use the unknown data. Depending on the known input, we might be able to make decisions like whether we want to get rid of a loop, if for example, it is degenerate for most iterations. The result is a new program that is specialized to the known data. 

## Deferring unifications in miniKanren
These ideas can be applied to miniKanren by introducing stages: some unifications are done in the first stage, while others are quoted out and get deferred to the second stage. The second stage represents code that is "kept for later", while the first stage is for executing now. Note that we can keep the entire code for later, but this defeats the purpose of staging. Given a unification, we can lift it by putting inside a quote, which we can escape with a comma. (This is just scheme, maybe unnecessary) Deferring a unification is similar to the "write" command in partial evaluation (replace with deferring a command in functional programming). Staging is not automatic, so we need to figure out a system for deciding which unifications get deferred. 

(First show how to do things manually, using lift, l==, simple examples with hand annotations)

## What needs to be deferred
Suppose we have an unknown variable in a unification. To decide whether the unification needs to be deferred we can examine the terms being unified to see whether they contain unknown data. Thus, we need to clarify two things: what is unknown data, and how do we decide whether a term containing unknown data is unknown? MiniKanren has its own logic variables, and then we have variables that are arguments to some functions, let expressions etc. If we have a expression like run * (q) ... would it ever benefit us to defer some unifications involving q? Then there is the question of how sticky we want these variables to be. If x is dynamic, we can declare so by writing (dynamic x). It is important to not defer (dynamic x) because otherwise terms containing x might be mistakenly treated as not dynamic. 


## Dynamic variable (talks about it, we tried but decided not to pursue, because it didn't work, and why)
The following example demonstrates how deferring unifications works:

```
(run* (q) (fresh (x y)
                (== q (list x y))
                (dynamic x)
                (== 1 x)
                (== x 6)))
```

The result is a generated quote:

```
'(((_.0 _.1) !! ((== _.0 '1) (== _.0 '6)))))
```

The unifications get deferred so we have a generated non-empty quote.
When deferring code, it’s useful to know whether the terms involved are dynamic. For example, if we just say that a term containing a dynamic variable is dynamic, then an expression like (cdr (cons 5 y)) where only y is dynamic will be treated as dynamic, which is not ideal.
