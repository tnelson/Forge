#lang forge

option run_sterling off


--Tests that it is possible to declare mutually recursive sigs without error

sig B {
	bToA : set A
}

sig A {
	aToB : set B
}
