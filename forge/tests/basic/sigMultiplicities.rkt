#lang forge

sig Thing {}
one sig SpecialThing {}

sig Stuff {}

test expect {
    onesigisntpersistent : {#Thing = 1 and #Stuff = 2} is sat
    onesigenforced1 : {#SpecialThing = 2} is unsat
    onesigenforced2 : {#Thing = 2} is sat
}