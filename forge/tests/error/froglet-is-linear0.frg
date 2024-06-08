#lang froglet
option run_sterling off

sig Node {
    next: lone Node
}

pred pp {
    next is linear
}

