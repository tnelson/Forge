#lang br/quicklang

(require brag/support)
(require forge2/tokenizer)

(for-each println (apply-tokenizer-maker make-tokenizer #<<LABEL

module tour/addressBook1

sig Name, Addr {}
sig Book {
  addr: Name -> lone Addr
  }

LABEL
))