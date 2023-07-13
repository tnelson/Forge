#lang forge


one sig Program {
    courses: set Course
}

abstract sig Course {
    reqs: set Course
}

one sig ECE155, ECE240, ECE250, ECE351 extends Course {}

pred prerequisites {
    reqs = ECE240->ECE155 + ECE250->ECE155 + ECE351->ECE250
}

fun graduationPlan: Program {{
    p: Program {
        #p.courses = 2
        all c: p.courses | some c.reqs implies c.reqs in p.courses
    }
}}

pred showSuccessfulPlan {
    prerequisites and some graduationPlan
}

run showSuccessfulPlan