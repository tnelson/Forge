#lang forge/core

(sig A)

(test my-test #:preds [(some A)]
              #:scope ([A 1 4])
              sat)

(check my-check #:preds [(some A)]
                #:scope ([A 1 4]))
