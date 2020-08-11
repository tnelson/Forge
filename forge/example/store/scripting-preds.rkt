#lang forge/core

(define-syntax (Build stx)  
  (syntax-case stx ()    
    [(Build)     
     #`(pred #,(syntax->datum stx bar) true)]))

(Build)
bar