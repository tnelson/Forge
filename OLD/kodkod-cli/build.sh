#!/bin/bash

rm -rf classes/*
rmdir classes
mkdir classes
javac -cp "jar/*" src/kodkod/cli/* test-sat4j/kodkod/cli/*.java -d classes

cd classes
jar cvf ../jar/kodkod-cli.jar kodkod/*
cd ..
