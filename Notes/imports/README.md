# Demo: requiring via filename vs. module

Run: `raco pkg install imports`

* `main.rkt` imports `shared.rkt` via filename;
* `other.rkt` imports `shared.rkt` via module.

Oddly, running via `racket main.rkt` looks fine. We see:

```
5 5
5 5
```
(there are two values because I wrote one with boxes and one without).

But running via DrRacket's run button with `main.rkt` open produces:
```
Welcome to DrRacket, version 8.10 [cs].
Language: racket/base [custom]; memory limit: 2048 MB.
0 0
5 5
> 
```
