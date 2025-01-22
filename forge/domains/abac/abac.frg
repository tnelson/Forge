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
  training: one True 
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
    reqS_rel: one Subject,
    reqA_rel: one Action,
    reqR_rel: one Resource
}
///////////////////////
// Domain predicates
///////////////////////

