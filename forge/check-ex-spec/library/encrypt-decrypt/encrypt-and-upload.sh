#!/bin/bash

racket encrypt-file.rkt $1.rkt $2/$1 $3

cd ../$
git add $1
git commit -m "Uploading $1."
git push