#lang forge

/* 
  this file is just like address.rkt but 
  - modified to fit our constraints while satisfying Alloy syntax
    - no asserts
    - no default bounds
  - modified for coverage of syntax cases
  - showing S-expression escape hatch
*/


module tour/addressBook1 

/*$(declare-sig animal) (declare-sig vehicle)*/

sig Name, Addr {} 
abstract sig Book extends Name {addr: Name -> lone Addr}

pred show (b: Book, a: Addr) { 
  #b.addr > 1
  #Name.(b.addr) > 1
  }
// addr1 : run {show} for 3 Name, 3 Addr, exactly 1 Book
/*
pred add (b, b': Book, n: Name, a: Addr) {b'.addr = b.addr + n -> a} 
pred del (b, b': Book, n: Name) {b'.addr = b.addr - n -> Addr}
fun lookup (b: Book, n: Name): set Addr {n.(b.addr)}

pred showAdd (b, b': Book, n: Name, a: Addr) { 
  add [b, b', n, a]
  #Name.(b'.addr) > 1
  }
addr2 : run {showAdd} for 3 Name, 3 Addr, exactly 1 Book

pred delUndoesAdd {
  all b,b',b": Book, n: Name, a: Addr |
    no n.(b.addr) and
    add [b,b',n,a] and del [b',b",n] implies b.addr = b".addr 
  }

pred addIdempotent {
  all b,b',b": Book, n: Name, a: Addr |
    add [b,b',n,a] and add [b',b",n,a] implies b'.addr = b".addr 
  }

assert addLocal {
  all b,b': Book, n,n': Name, a: Addr |
    add [b,b',n,a] and n != n'
      implies lookup [b,n'] = lookup [b',n']
  }

addr3 : check {delUndoesAdd} for 10 Name, 10 Addr, exactly 3 Book
*/
