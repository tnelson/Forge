#lang forge

/*
  Example of defining a trace skeleton using `inst` manually.
*/

option verbosity 10

sig State { successor: set State }
sig Initial extends State {}

inst myExample {
    State = `State0 + `State1 + `State2 + `State3
    Initial = `State3
    successor = `State3->`State0 + `State0->`State0 + `State0->`State3 + 
                `State1->`State2 + `State2->`State1 + `State1->`State1 + 
                `State2->`State2
}

run {} for myExample
