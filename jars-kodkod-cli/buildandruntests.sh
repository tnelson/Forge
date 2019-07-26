#!/bin/bash

javac -cp "jar/*:jar/for_tests/*" test-sat4j/kodkod/cli/*.java -d classes
java -cp "jar/*:jar/for_tests/*:classes" org.junit.runner.JUnitCore kodkod.cli.AllParserTests
