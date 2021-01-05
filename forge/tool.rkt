#lang racket/gui

;; Much of this is lifted from RackUnit's DrRacket integration tool.
;;   by Tim in January 2021
;; There is a lot of extra code leftover from our tool experiments that I'm keeping in
;;   for reference in case it becomes useful later.

; c:\Program Files\racket\share\pkgs\rackunit-gui\rackunit\private\gui\drracket-link.rkt


(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         framework)

;(require "sigs.rkt")
(define LINK-MODULE-SPEC 'forge/drracket-link)
(provide tool@)
 
(define tool@
  (unit
    (import drracket:tool^ )
    (export drracket:tool-exports^)
    (define (phase1) void)
    (define (phase2) void)
    (define highlight-color-1 (make-object color% 207 255 207))
    ;(define highlight-color-2 (make-object color% 207 207 255))
    ;(define hl-thunk-1 #f)
    ;(define hl-thunk-2 #f)
    ;(define run-choice #f)

    ; mixing into a frame (so we should have access to all the frame methods)
;    (define ping-button-mixin
;      (mixin (drracket:unit:frame<%>) ()
;        (super-new)
;        (inherit get-button-panel
;                 get-definitions-text)
;        (inherit register-toolbar-button)
;
;        (define button-bitmap
;          (let* ((bmp (make-bitmap 16 16))
;                 (bdc (make-object bitmap-dc% bmp)))
;            (send bdc erase)
;            (send bdc set-smoothing 'smoothed)
;            (send bdc set-pen "red" 1 'transparent)
;            (send bdc set-brush "red" 'solid)
;            (send bdc draw-rectangle 2 2 8 8)            
;            (send bdc set-bitmap #f)
;            bmp))         
;        
;        (let ((btn
;               (new switchable-button%
;                    (label "Highlight")
;                    (callback (λ (button)
;                                
;                                
;                                (send (get-definitions-text) begin-edit-sequence)
;                                ;(send (get-definitions-text) set-position 0)
;                                ;(send (get-definitions-text) insert "X")
;                                (define lp (send (get-definitions-text) last-position))
;                                (define midp (/ lp 2))
;                                (cond
;                                  [hl-thunk-1 (hl-thunk-1)
;                                              (hl-thunk-2)
;                                              (set! hl-thunk-1 #f)
;                                              (set! hl-thunk-2 #f)]
;                                  [else (set! hl-thunk-1 (send (get-definitions-text) highlight-range 0 midp highlight-color-1))
;                                        (set! hl-thunk-2 (send (get-definitions-text) highlight-range (+ midp 1) lp highlight-color-2))])
;                                
;                                (send (get-definitions-text) end-edit-sequence)
;                                ;(message-box "Testing"
;                                ;             (format "choice: ~a curr-state: ~a"
;                                ;                     (send run-choice get-selection)
;                                ;                     forge:curr-state))
;                                ))
;                    (parent (get-button-panel))
;                    (bitmap button-bitmap))))
;          (register-toolbar-button btn #:number 11)
;          (send (get-button-panel) change-children
;                (λ (l)
;                  (cons btn (remq btn l)))))))

    
;    (define mixin-run-choice
;      (mixin (drracket:unit:frame<%>) ()
;        (super-new)
;        (inherit get-button-panel
;                 get-definitions-text)       
;        (let ((ch
;               (new choice% ;switchable-button%
;                    
;                    [label "Run"]
;                    [choices (list "foo1: run {} for 3"
;                                   "foo2: run {pred1} for 5"
;                                   "foo3: run {pred2} for 2")]
;                    [parent (get-button-panel)]
;                    [stretchable-width #f]
;                    [callback
;                     (λ (choice evt)
;                       ;(define selection (send choice get-selection)) 
;                       (void))])))
;          (set! run-choice ch))))
              
;    (define mixin-menu
;      (mixin (drracket:unit:frame<%>) ()
;        (super-new)
;        (inherit get-button-panel
;                 get-definitions-text
;                 get-language-menu
;                 get-menu-bar)
;       
;        (let ((ch
;               (new menu%                    
;                    [label "Forge"]                    
;                    ;[parent (get-language-menu)])))
;                    [parent (get-menu-bar)])))
;                    
;          (void))))
    
    
    (define unit-mixin
       (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-interactions-text)
        (inherit register-toolbar-button)

        (define (do-forge-highlight pos-start pos-end a-color)
          (send (get-definitions-text) begin-edit-sequence)          
          (send (get-definitions-text) highlight-range pos-start pos-end a-color)
          (send (get-definitions-text) end-edit-sequence))
        
        (define/private (setup-helper-module)
          (let* ([interactions (get-interactions-text)]
                 [link (parameterize ((current-namespace (send interactions get-user-namespace)))
                        (dynamic-require LINK-MODULE-SPEC 'link))])
            (set-box! link (vector do-forge-highlight))
            (printf "set link box: ~a~n" link)))

         ; RackUnit extended the REPL, not the frame. 
        ;(define/override (reset-console)
        ;  (super reset-console)
        ;  (setup-helper-module))
         ;(setup-helper-module)

         (define button-bitmap
           (let* ((bmp (make-bitmap 16 16))
                  (bdc (make-object bitmap-dc% bmp)))
             (send bdc erase)
             (send bdc set-smoothing 'smoothed)
             (send bdc set-pen "red" 1 'transparent)
             (send bdc set-brush "red" 'solid)
             (send bdc draw-rectangle 2 2 8 8)            
             (send bdc set-bitmap #f)
             bmp))

         (let ((btn
               (new switchable-button%
                    (label "Helper")
                    (callback (λ (button) (setup-helper-module)))
                    (parent (get-button-panel))
                    (bitmap button-bitmap))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))
    
    (drracket:get/extend:extend-unit-frame unit-mixin)
    
    ;(drracket:get/extend:extend-unit-frame ping-button-mixin)
    ;(drracket:get/extend:extend-unit-frame mixin-run-choice)
    ;(drracket:get/extend:extend-unit-frame mixin-menu)

    
    ))

; At forge/core REPL:
; (require racket/gui/base)
; ((vector-ref (unbox link) 0) 0 10 (make-object color% 207 255 207))
