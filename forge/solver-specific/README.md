Forge's solver backend pipeline looks roughly like this:

* `sigs.rkt`, `sigs-functional.rkt`, or some other module creates a `Run`, and invokes `send-to-solver` in `send-to-solver.rkt`, which actually performs most of the pre-solver computation, such as resolving bounds. 
* `send-to-solver` invokes a different back end depending on options selected. 
  + The `pardinus` backend creates a persistent worker process which listens for problem specifications, requests for instances, evaluation, etc. 
  + The `smtlib` backend assumes that the user has `cvc5` on their path. The SMT-LIB text created will vary based on options (e.g., using theory of relations vs. uninterpreted functions). 
  + The `fortress` backend does not yet exist, but it would also be split off in this way.
* Each backend has its own format for reporting results back to Forge, so each needs its own conversion modules.