#lang forge

option backend smtlibtor

---------------- Signatures ----------------

abstract sig Person {}

sig Faculty extends Person {}

abstract sig Student extends Person {
  id: one Id,
  transcript: set Course
}

sig Graduate, Undergrad extends Student {}

sig Course {
  taughtby: one Person, -- (MODIFIED)
  enrolled: set Student,
  waitlist: set Student, 
  prerequisites: set Course
}

sig Id {}

---------------- Fact ----------------

pred model_facts 
{
  -- forcing 'some' on enrolled (MODIFIED)
  all c: Course | some c.enrolled

  -- All instructors are either Faculty or Graduate Students (MODIFIED)
  all c: Course | c.taughtby in Faculty+Graduate 

  -- No one is waiting for a course unless someone is enrolled
  all c: Course | some c.waitlist => some c.enrolled

  -- Graduate students do not teach courses they are enrolled in
  -- or wainting to enroll in
  all c: Course | c.taughtby !in c.enrolled + c.waitlist

  -- No student is enrolled and on the waitlist for the same course
  all c: Course | no (c.enrolled & c.waitlist)

  -- No two distinct students have the same ID
  all s1, s2: Student | s1 != s2 => s1.id != s2.id

  -- A student can only have a course for which they have the prerequisites
  all s: Student | s.transcript.prerequisites in s.transcript

  -- There are no cycles in the prerequisite dependencies 
  all c: Course | c !in c.^prerequisites
}


---------------- Assertion ----------------
-- A student can only wait to be in a course for which they have the prerequisites
pred AllWaitsHavePrereqs {
     all s: Student | (waitlist.s).prerequisites in s.transcript
}

-- ALLOY-TO-FORGE NOTE: this was originally a failing `check`:
test expect {
    academia_0: {not {model_facts => AllWaitsHavePrereqs}} for 30 is sat
}









