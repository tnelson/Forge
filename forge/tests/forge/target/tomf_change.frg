#lang forge 

/*
  Let's test integer minimization in a different sort of problem,
  making change. This will involve a larger bitwidth and has a 
  different sort of feel to it. 

  We won't try to optimize the numeric reasoning here: the value 
  of coins is set with constraints, not a partial-inst optimizer. 
*/

option problem_type target
option solver PMaxSAT4J

abstract sig Coin {}
sig Penny, Nickel, Dime, Quarter extends Coin {}

one sig Transaction {
    change: one Int,
    used: set Coin
}

fun valueOf[c: Coin]: one Int {
    c in Penny => 1 else 
    c in Nickel => 5 else 
    c in Dime => 10 else 25
}

pred correctChangeMade {
    // We assume the set of Coin atoms corresponds to the available 
    // drawer of change. So we just check the total:
    (sum c: Transaction.used | valueOf[c]) = Transaction.change
}
pred correct57 {
    // Return $0.57 in change
    Transaction.change = 57
    // and do so correctly
    correctChangeMade
    // with a specific drawer state 
    #Quarter = 2
    #Dime = 2
    #Nickel = 3
    #Penny = 8
}

////////////////////////////////////////////////////////////////////////////

// Minimize the number of coins used. This will produce 
//  Instance 1: 5 coins (2 quarters, 1 nickel, 2 pennies)
//  Instance 2: 7 coins (1 quarter, 2 dimes, 2 nickels, 2 pennies)
//    (There is no way to solve this with exactly 6 coins, given the drawer.
//     We need 2 pennies at minimum, leaving us only 4 to make 55 cents.)
//  Instance 3: 9 coins (2 quarters, 7 pennies)
change57_min_coins: run {correct57} for 8 Int, exactly 15 Coin
  target_int {#Transaction.used} close_noretarget 

////////////////////////////////////////////////////////////////////////////

// Maximize the number of pennies used (someone paid with multiple 
// rolls earlier for some reason)
// Instance 1:    (1 quarter, 1 dime, 3 nickels, 7 pennies)

change57_max_1_5: run {correct57} for 8 Int, exactly 15 Coin
  target_int {#(Transaction.used & (Penny + Nickel))} far_noretarget 

////////////////////////////////////////////////////////////////////////////

// ? Can we express "it is impossible to...?" This seems like H.O. universal.
