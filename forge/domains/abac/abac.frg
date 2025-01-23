#lang forge/froglet 

/*
    Domain model for attribute-based access control, specialized to the setting of the 
    Logic for systems ABAC homework. This file is imported by the runner for the 
    #lang forge/domains/abac DSL. 
*/

///////////////////////
// Datatype definitions 
///////////////////////

// We'll model booleans as presence/non-presence of an entry in a partial function.
one sig True {}
// Notice that there's no "one sig False {}". One reason is that it's not necessary. 
// But a better reason is that if we have False we'd need _total_, not partial, functions,
// which would make writing concrete examples more verbose. 

// Components of requests (S, A, R)
sig Subject, Action, Resource {}

// Subjects: the entity requesting access
sig Customer extends Subject {}
sig Employee extends Subject {
  // Notice we don't say "one Boolean" here.
  // We *could*, but in *this* specific example it might make testing more elaborate. 
  training: lone True 
}
sig Accountant, Admin extends Employee {}

// Actions: the nature of the access
sig Read, Write extends Action {}

// Resources: the target of the requested access
sig File extends Resource {
    // Every File is owned by some entity. Note that we don't allow shared ownership.
    owner: one Subject,
    // Every file is under audit or not. Again, notice that we don't say "one Boolean".
    audit: lone True
}

// This model works with a single request at a time
one sig Request {
    // Every request has a single subject, action, and resource.
    reqS: one Subject,
    reqA: one Action,
    reqR: one Resource
}

///////////////////////
// Policy predicates
///////////////////////

// These are currently all done in the parser/expander for #lang forge/domains/abac
// But let's try to approximate them in Froglet! Here are two very basic policies:

/*
policy original
  permit if: s is admin, a is read, r is file.
end;

policy modified
  permit if: s is admin, a is read, r is file.
  permit if: s is accountant, a is read, r is under-audit.
end;
*/

// TODO: problem: Froglet disables "in", but we need a type predicate.

pred original_permits_if[s: Subject, a: Action, r: Resource] {
  // TASK: write a constraint that evaluates to true exactly when the 'original'
  // policy would permit the request.
  s in Admin and a in Read and r in File
}

pred modified_permits_if[s: Subject, a: Action, r: Resource] {
  // TASK: write a constraint that evaluates to true exactly when the 'modified'
  // policy would permit the request.
  (s in Admin      and a in Read and r in File) or
  (s in Accountant and a in Read and r.audit = True)
}

// This won't run when the file is imported by the abac language. It will only 
// run (and load the visualizer) if the Forge file is run directly.
difference_original_modified: run {
  // TASK: write a constraint that evaluates to true exactly when the two policies
  // disagree on permitting some request.
  some s: Subject, a: Action, r: Resource | {
    not (original_permits_if[s,a,r] iff modified_permits_if[s,a,r])
  }
}
