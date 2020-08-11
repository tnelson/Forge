#lang forge/core

(require "forge-core-file.rkt") ; this will display the run from the other file :(

(pred PopularAKing
  (all ([b B]) (in (-> AKing b) friend)))

(run new-run #:preds [
  (all ([a A]) (some (getFriends a)))
  (all ([b B]) (some (join friend b)))
  (AreFriends AKing BKing)
  PopularAKing])

(is-sat? new-run)
(define instance-stream (Run-result new-run))
(display (stream-first instance-stream))