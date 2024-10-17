#lang forge

option backend smtlibtor
option verbose 0

abstract sig Object {}

sig Dir extends Object {contents: set Object}

one sig Root extends Dir { }

sig File extends Object {}

pred model_facts {
	Object in Root.*contents
}

pred SomeDir {
	all o: Object - Root | some contents.o
	}

test expect {
    fs_sd: {model_facts => SomeDir} is theorem
}
