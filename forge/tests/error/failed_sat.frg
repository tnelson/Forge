#lang forge
option run_sterling off
option verbose 0

sig Cell {}

test expect {    
    foo: {Cell != Cell} is sat
}
