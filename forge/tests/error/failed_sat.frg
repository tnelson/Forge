#lang forge
option verbose 0

sig Cell {}

test expect {    
    foo: {Cell != Cell} is sat
}
