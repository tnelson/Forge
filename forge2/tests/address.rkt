#lang forge2

module tour/addressBook1

sig Name, Addr {}
sig Book {
  addr: Name -> lone Addr
  }