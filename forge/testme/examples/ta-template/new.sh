#!/bin/bash

source config.sh

ASSIGNMENT=$ASSIGNMENTS/$1

if [ ! $ASSIGNMENT ]; then
    echo "$ASSIGNMENT already exists."
    exit 1
fi

if [ "$1" = "" ]; then
    echo "Usage: bash new.sh <assignment name>"
    exit 1
fi

mkdir -p $ASSIGNMENT/wheats
mkdir -p $ASSIGNMENT/chaffs
mkdir -p $ASSIGNMENT/tests
