#lang forge

-- Represent a single run of the protocol 

abstract sig Phase {}
one sig Error extends Phase {}
one sig One extends Phase {}
one sig Two extends Phase {}
one sig Three extends Phase {}

abstract sig Agent {}
one sig Tim extends Agent {}
one sig Bank extends Agent {}
one sig Eve extends Agent {}

abstract sig Datum {}
sig DName extends Datum {dname: one Agent}
sig DNonce extends Datum {dnonce: one Agent} -- abstract out agent-secret separation

sig Step {
  agentState: set Agent -> Phase
}

one sig Sol {
  nextStep: set Step -> Step
}

fact nextStep: linear

-- No, it's a func PER Step
--fact agentState: func

sig Message {
  src: one Agent,
  dst: one Agent,
  key: one Agent, -- abstract out PKI, key-agent separation, etc.
  contents: set Datum
}

pred structural {
    -- TODO: [] join
  all s: Step | all a: Agent | one p: Phase | a->p in s.agentState

  -- TODO: abstract
  all d: Datum | d in DName + DNonce
}

run {structural} for 3 Agent, 6 Message, 8 Datum, 6 Step, 1 Sol
