#lang forge

sig A { edges: set A }

test expect { foo: {A} is sat}
