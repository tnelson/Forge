#!/bin/bash

cd jar
java -cp "./*" kodkod.cli.KodkodServer ../$1
cd ..
