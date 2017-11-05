# staged-miniKanren

This project explores multi-stage logic programming in miniKanren.
While partial evaluation has a rich history in both functional and logic programming,
multi-stage programming has so far only been explored in a functional/imperative setting,
with many success stories in high-performance computing.
How can we add staging constructs to a logic / relational programming language like miniKanren?
And what can we do with such constructs?
Can we turn an interpreter for a functional language written in miniKanren into a compiler translating any functional program into a relational one?
Can we go beyond and collapse towers of interpreters, covering hybrid paradigms?
What happens when a staged program is run "backwards"?
These are the sort of questions we want to answer.
