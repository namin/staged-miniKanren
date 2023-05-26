#lang racket

Pink has:
(car (code e)) -> (code (car e))
(+ (code e1) (code e2)) -> (code (+ e1 e2))

We can imagine extending it with rules like:
(+ v (code e2)) -> (code (+ (lift v) e2))

If we did, we could write an evaluator:
(define (eval e)
  ...)

And then write queries like this, where we place later-stage
variables in for not-yet-known expressions:

(eval `(+ (+ 1 2) ,(code e)))

I think that would evaluate to:
(code (+ 3 (eval e)))


What's something that would be truly relational, not functional? Amb!
Here evalo is not a function because it has two values for the amb expression:

(evalo (amb 1 2) 1)
(evalo (amb 1 2) 2)

I was hoping that with condg we could use information known about the value at
staging time to specialize such a branch, but it seems as though we are being
forced to choose a mode.

By choosing a mode we are making sure that...



If you chose the `v` as the input, you'd defer everything to runtime.

(defrel (evalo-staged e v)
  (condg
   #:fallback ...
   ([]                                ;; quote
    [(== v '())]
    [(later (== e '(quote ())))])
   ([e1 e2 v1 v2]
    [(== v `(,v1 . ,v2))]             ;; cons
    [(later (== e `(cons ,e1 ,e2)))
     (evalo-staged e1 v1)
     (evalo-staged e2 v2)])
   ([e-pair res v2]                   ;; car
    [(== res `(,v . ,v2))]
    [(later (== e `(car ,e-pair)))
     (evalo-staged e-pair res)])))

(run 1 (e)
  (staged
   (evalo-staged e 5)))

(evalo-staged e-pair `(5 . ,v2))

The `car` and `cons` cases both apply, so the interpreter will fall back after
one recursion in this particular example.


Nada: same with non-moded interpreter:

(defrel (evalo-staged e v)
  (condg
   #:fallback ...
   ([]
    [(== e '(quote ())) (== v '())] [])
   ([e1 e2 v1 v2]
    [(== e `(cons ,e1 ,e2)) (== v `(,v1 . ,v2))]
    [(evalo-staged e1 v1)
     (evalo-staged e2 v2)])
   ([e-pair res v2]
    [(== e `(car ,e-pair)) (== res `(,v . ,v2))]
    [(evalo-staged e-pair res)])))

Sort of coincidental that it happens to work in this case.

Possible property: is the relation a function considering when the staging-time arguments
as the function arguments and the runtime arguments as the function return values?


Considering how this property fails to hold for staging the interpreter backwards, with
value as staging-time and expression as runtime...

One problem is that this is not a function: there are multiple expressions for any given
value. General consequence: we may usually fall back.

Ex. the answer is (1 2). Could be cons, list, among others. So we will fall back rather than
specialize.


Q. can we get the divergence problem without having the output be constrained?

Nada: In some sense the fact that we fall back is just luck. That there is more
than one case that applies.

Additional properties of the interpreter:

The staging-time portion is structurally recursive.
 
? When there's a hole in the expression, it always leads to multiple answers if the output is
unconstrained

Not quite here:
(run 1 (q e v)
  (staged
   (evalo-staged `((lambda (,q) x) 5) v)))

There's a hole in the expression but we still get exactly one answer. Though the hole is not
in an expression position.


If we didn't make the callo in the application case `later`, we'd diverge when interpreting
omega.


Nada q: can there be an interpreter that terminates for every ground expression, but diverges
when you put a hole in somewhere?

Related michael thought: I wanted it to be true of the fallback search that when g terminates
for every ground assignment of its variables and fb always terminates, then (fallback fb g)
always terminates.

(defrel/generator (always-fails e)
  (fallback
   fbg
   (fresh (e1 e2)
     (== e `(,e1 ,e2))
     (g e2))))

For all finite `e` `always-fails` fails.

For unknown `e`, `always-fails` diverges.


For the interpreter...
- Fails for syntactically invalid expressions, unbound variables.
- Otherwise does not fail at staging time. If we were to unify with the value at staging time,
that introduces more opportunity for failure and thus divergence like above.

So we say don't write `always-fails` because, it always fails given ground.

Here's a version that has a base case, but still has the same problem:

(defrel/generator (always-fails2 e)
  (fallback
   fbg
   (conde
     [(== e '())
      fail]
     [(fresh (e1 e2)
       (== e `(,e1 ,e2))
       (g e2))])))

What about this?

(eval-staged `(map (lambda () (5 5)) ,e) v)

Not a problem because the map happens at runtime.


Eval for language with only list and variable reference.
Call the interpreter with an unknown expression and an empty environment.

(eval-only-lists e '() v)

It can try the list constructor every time, but because the environment is
empty the only base case of environment lookup will always fail.

I think that with the fallback search, this query will diverge at staging-time.

This is another one that fails for all ground e.

TODO: is this situation enough to make us think that nondeterminism-based fallback
is a bad idea?

Key TODO: A worse case would be a relation that succeeds for some ground `e`, fails for
others, and our staging diverges. Is this possible?



Given that we're forced to choose a mode and only handle things that act like
functions in the staging-time portion, what's different from functional staging?


1. Deal with holes w/ fallback. Holes introduce nondeterminism even when dealing
with functions. (There's the possible analog above with Pink, but no motivation for it
  without relational execution at runtime.)

2. Cross-stage persistence of values that are not completely known
      - type constraints
      - learning information later, after persisting

Aside: note that although this is about dealing with function arguments with holes, not more
generally relations at staging-time, our relational interpreter does do better than a functional
+ deferring interpreter like above. Consider:

(run 1 (q)
  (staged
   (evalo-staged `((lambda (,q) x) 5) 5)))

Here (modulo a current bug) we determine `q` is `x` at staging time! A functional version using
an extended Pink would defer eval to runtime before it even managed to commit to the pattern match
of lambda.

Note that terms and term variables don't need to be assigned a stage.
      
3. partial-apply, apply-partial to add just enough first-class code to reuse staged code for
interpreted closure bodies without breaking relationality.




Imagine someone trying to write a program synthesizer given a semantics, and also a code generator
to specialize the synthesizer for known program parts. What are the concerns they have to deal with?

1. Which parts of the program do we specialize against, and which are handled by the runtime
synthesizer? This could be particularly tricky if the unknown parts are not quite at a convenient
subexpression position (ex. unknown function argument name or argument list)

2. How do I make sure that the generated and runtime portions have the same semantics?

3. How do I have values that can flow at runtime between generated code and runtime synthesizer?
In particular, what about values used to represent higher-order elements of the language under
interpretation. Some of these values need to call the runtime synthesizer because nothing about
them was known to specialize against, and some of these need to call generated code. But the
representation has to be the same, so that it can be used at any call-site.





