#lang forge

option backend smtlibtor
---------------- Signatures ----------------

abstract sig Person {}

sig Faculty extends Person {
  incommittee: one Graduate 
}

abstract sig Student extends Person {
  id: one Id,
  transcript: set Course,
  major: one Department
}

sig Undergrad extends Student {}

sig Graduate extends Student {
  advisor: one Faculty 
}

one sig Helper {
    department : pfunc Person -> Department
}

sig Course {
  taughtby: one Person,
  enrolled: set Student,
  waitlist: set Student, 
  prerequisites: set Course
}

sig Id {}

sig Department { 
  courses: set Course, 
  required: set Course 
} 

---------------- Fact ----------------

pred field_facts {
    all c : Course | c.taughtby in Graduate + Faculty
    all c : Course | some c.enrolled
    all d : Department | some d.courses and some d.required
    all p : Person | p in Graduate + Faculty => p in (Helper.department).Department
    all p : Person | p not in Graduate + Faculty => p not in (Helper.department).Department
}

pred model_facts  {
  -- No one is waiting for a course unless someone is enrolled
  all c: Course | some c.waitlist => some c.enrolled

  -- Graduate students do not teach courses they are enrolled in
  -- or wainting to enroll in
  all c: Course | c.taughtby !in c.enrolled + c.waitlist

  -- No student is enrolled and on the waitlist for the same course
  all c: Course | no (c.enrolled & c.waitlist)

  -- No two distinct students have the same ID
  all s1,s2: Student | s1 != s2 => s1.id != s2.id

  -- There are no cycles in the prerequisite dependences 
  all c: Course | c !in c.^prerequisites

  -- A student can only have, wait for or take a course for which they have the prerequisites
   all s: Student | (waitlist.s + enrolled.s + s.transcript).prerequisites in s.transcript

  -- Each department has at least one instructor
  all d: Department | some department.d

  -- Each course is in a single department
  all c: Course | one c.~courses and one c.~required

}


---------------- Assertion ----------------

-- No instructor is on the waitlist for a course that he/she teaches
pred NoWaitingTeacher {
  all c: Course | no (c.taughtby & c.waitlist)
}

test expect {
    academia_3 : {(field_facts and model_facts) => NoWaitingTeacher} for 30 is theorem
}
