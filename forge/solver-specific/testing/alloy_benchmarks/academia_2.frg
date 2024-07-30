#lang forge

option backend smtlibtor

---------------- Signatures ----------------

abstract sig Person {}

sig Faculty extends Person {}

abstract sig Student extends Person {}

sig Graduate, Undergrad extends Student {}


sig Course {
  taughtby: one Person,
  enrolled: set Student,
  waitlist: set Student
}

fun teaches : Person -> Course { ~taughtby } -- what is the point of this?

---------------- Fact ----------------
pred field_facts {
    all c : Course | some c.enrolled
}

pred model_facts
{
  -- All instructors are either Faculty or Graduate Students
  all c: Course | c.taughtby in Faculty+Graduate 

  -- No one is waiting for a course unless someone is enrolled
  all c: Course | some c.waitlist => some c.enrolled

  -- Graduate students do not teach courses they are enrolled in
  -- or wainting to enroll in
  all c: Course | c.taughtby !in c.enrolled + c.waitlist
}


---------------- Assertion ----------------

-- No student is enrolled and on the waitlist for the same course
pred NoEnrolledAndWaiting {
  all c: Course | no (c.enrolled & c.waitlist)
}

test expect {
    academia_2 : {(field_facts and model_facts) => NoEnrolledAndWaiting} for 30 is theorem
}











