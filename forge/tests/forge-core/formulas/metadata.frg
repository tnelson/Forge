#lang forge

-- Surface Forge can't express tests about metadata on AST nodes, but forge/core can.
-- Hence, import this surface file into forge/core tests for checking there.

option verbose 0
option run_sterling off

pred pred_surface[x,y: lone Int, z: univ] {}
