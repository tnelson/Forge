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

https://console.cloud.google.com/home/dashboard?project=pyret-examples&authuser=2


## Cloud Function

Used to have a cloud function

- name = submit
- trigger = https://us-central1-pyret-examples.cloudfunctions.net/submit
- source = index.js (here)
- deps = package.js (here)
