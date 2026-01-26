check-ex-spec
===

2022-01-21: ben g says this code is usused

Simple backup logging for `check-ex-spec`. Here's the idea:

- keep an append-only log file with file snapshots, etc.
- write 1 log per check-ex-spec run
- try to post the current log & any failed logs after a run

Handle all errors & do this before the normal forge logging.

There are two goals:

1. Use a simpler DB schema that is less helpful but also less likely to fail
   (at least until the normal forge logging is all debugged)
2. Keep a count of all created logs so the DB can tell if any are missing.

Hope to delete this backup eventually,
Ben G


## Cloud Function

Used to have a cloud function

- name = cs1710_check-ex-spec ?
- trigger = https://us-east1-pyret-examples.cloudfunctions.net/cs1710-2021-examplar
- source = index.js (here)
- deps = package.js (here)
