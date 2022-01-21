2022
===

Simple logging for 2022.

Goal = capture programs & errors.

Implementation idea:

- keep a local append-only log with file snapshots
- write 1 log to this file per run (of anything: cxs or otherwise)
- try to post the current log & any failed logs after a run

(based on forge/logging/check-ex-spec/)


## GCloud URL

xxx

## Cloud Function

Used to have a cloud function

- name = lfs2022
- trigger = https://us-east1-pyret-examples.cloudfunctions.net/lfs2022
- source = index.js (here)
- deps = package.js (here)
