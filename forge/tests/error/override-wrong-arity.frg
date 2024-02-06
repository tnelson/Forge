#lang forge

sig Node { edges: set Node }

test expect { should_error: {some Node ++ edges} is sat}
