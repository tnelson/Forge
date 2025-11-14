#lang forge/temporal
/*
  This model documents a place that Forge disagrees with Alloy. 
  Pardinus applies a rewriting process to convert temporal formulas 
  to normal formulas referencing "timed" relations. Usually, these 
  rewrites are included in the mapping back to original formulas
  that cores rely on. Here, however, one is not: an "extra" formula 
  gets produced that has no attached location. 

  In Alloy, this model will not produce a core at all. 
  In Forge, it produces a core, with one "extra" formula 
    unattributed. This is expressed via an "[UNKNOWN]" annotation, 
    and then Forge prints the formula to console. 

  (as of May 2024 -- TN)

*/

option solver MiniSatProver
option core_minimization rce
option logtranslation 1
option coregranularity 1
sig Post {}
sig Server {var posts : set Post }
run { (posts)' != (posts)' }
