#lang racket

(require "../../../sigs.rkt")

(sig Abstract #:abstract)
(sig Extension1 #:extends Abstract)
(sig Extension2 #:extends Abstract)

(check abstractEnforced ((= Abstract (+ Extension1 Extension2))))
(test extensionsAllowed ((some Extension1)) 'sat)
(check emptyExtensionsEmptyAbstract ((=> (no (+ Extension1 Extension2)) (no Abstract))))


(sig Unextended #:abstract)

(test unextendedCanPopulate ((some Unextended)) 'sat)
