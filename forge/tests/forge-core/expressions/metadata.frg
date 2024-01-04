#lang forge

-- Surface Forge can't express tests about metadata on AST nodes, but forge/core can.
-- Hence, import this surface file into forge/core tests for checking there.

option verbose 0
option run_sterling off

fun helper_surface[x: lone univ, y: univ]: lone Int { x & y }
fun helper_surface_grouped[x, y: lone univ, z: univ]: lone Int { x & y & z }
