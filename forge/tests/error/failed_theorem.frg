#lang forge
option run_sterling offoption verbose 0

sig Cell {}

test expect {    
    foo: {Cell != Cell} is theorem    
}
