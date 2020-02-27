#lang forge

sig A {}

pred p {
    not not not not some A--.A
    
}

run p