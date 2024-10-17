#lang forge

option backend smtlibtor
option verbose 0

sig Title{}
sig Book {name: one Title}

abstract sig Copy{description: one Book}
sig Reference extends Copy {}
sig General extends Copy {}

abstract sig Borrower {checkedout: set General}
sig Student extends Borrower{}
sig Faculty extends Borrower{}

pred OneCheckOut {
all s: Student | #(s.checkedout) <= 3
all c: General | lone checkedout.c
}



pred NoMultCheckouts {all c: General | #checkedout.c < 2}
test expect { 
    library: {OneCheckOut => NoMultCheckouts} is theorem
}