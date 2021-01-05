;; Most of this is lifted from RackUnit's DrRacket integration tool.
;;   by Tim in January 2021

;; Written in #%kernel to avoid adding any module-attachment
;; dependencies. Initialized by the

(module drracket-link '#%kernel
  (#%provide link)

  #|

  If initialized (has non-#f value), the box should contain a vector
  of the following procedures:
  
  (vector get-errortrace-backtrace
          show-backtrace
          show-source)
  |#

  (define-values (link) (box #f)))
