2023
===

Simple logging for 2023.

Goal = capture programs & errors.

Implementation:

- keep a local append-only log with file snapshots
- write 1 log to this file per run of anything: cxs or otherwise
- try to post the current log & any failed logs after a run

(based on forge/logging/check-ex-spec/)


## GCloud URL

https://console.cloud.google.com/home/dashboard?project=pyret-examples&authuser=2

## MySQL DB

instance lfs2023
password drracket

mysql> use lfs2023;
mysql> select count(1) from main;
mysql> select student, project from main;


## Cloud Function

- name = submit
- trigger = https://us-central1-pyret-examples.cloudfunctions.net/submit
- source = index.js (here)
- deps = package.js (here)
