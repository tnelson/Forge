#lang forge
option run_sterling off
option verbose 0 

sig Node {edges: set Node}

test expect {
    shouldFailWithError: {} for {  
        Node in `N0 + `N1 + `N2
        Node ni `N3
    } 
    is sat
}
