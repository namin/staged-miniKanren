- [x] Use a reify tag for quasi instead of ad-hoc knowledge for each construct.
      Then `l==` becomes `(lift (== ,(reify a) ,(reify b)))`.

- [ ] Investigate how/if unstaged evalo should reenter staged code.
      Share closure representation and calling convention.
