#!/bin/bash

java -cp "jar/*" -Djava.library.path=./jar kodkod.cli.KodkodServer -stepper -target-oriented "$@"
