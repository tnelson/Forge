#lang forge

option backend smtlibtor
option verbose 0

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

fun teaches : Instructor -> Course { ~taughtby }

---------------- Fact ----------------
pred field_facts {
    all c : Course | c.taughtby in Graduate + Faculty
    all c : Course | some c.enrolled
}


pred model_facts
{
  -- No one is waiting for a course unless someone is enrolled
  all c: Course | some c.waitlist => some c.enrolled

  -- Graduate students do not teach courses they are enrolled in
  -- or wainting to enroll in
  all c: Course | c.taughtby !in c.enrolled + c.waitlist
}


---------------- Assertion ----------------
-- No instructor is on the waitlist for a course that he/she teaches
pred NoWaitingTeacher {
  all c: Course | no (c.taughtby & c.waitlist)
}

test expect {
    academia_4: {(model_facts and field_facts) => NoWaitingTeacher} is checked
}










