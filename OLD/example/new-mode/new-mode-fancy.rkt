#lang forge/new-mode

sig Box {
    contains: one Contents
}

sig Contents {}
one sig InitContents extends Contents {}
one sig TermContents extends Contents {}

run {}
