#!/bin/bash

source config.sh

if [ "$1" = "" ]; then
    echo "Usage: bash setup.sh <github repo url>"
    exit 1
fi

cd $COMPILEDS
git init
git remote add origin $1
git checkout -b master
git pull origin master
touch README
git add README
git commit -m "setup"
git push origin master

