#lang racket/gui

;; Much of this is lifted from RackUnit's DrRacket integration tool.
;;   by Tim in January 2021
;; There is a lot of extra code leftover from our tool experiments that I'm keeping in
;;   for reference in case it becomes useful later.

(require drracket/tool
         ; 
         ;drracket/tool-lib
         racket/class
         racket/gui/base
         racket/unit
         ;mrlib/switchable-button
         ;framework
         )

(define LINK-MODULE-SPEC 'forge/drracket-link)
(provide tool@)
 
(define tool@
  (unit
    (import drracket:tool^)
    
    (export drracket:tool-exports^)
    (define (phase1) void)
    (define (phase2) void)

    (define unit-setup-thunk (box #f))
    (define local-vector (box #f))
    
    (define unit-mixin
       (mixin (drracket:unit:frame<%>) ()      
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-interactions-text
                 open-in-new-tab)        

        ;;;;;;;;;;;;;;;;;;; FUNCTIONALITY TO EXPOSE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         
         (define (do-forge-highlight posn span path a-color key)           
           ;(printf "Forge highlight: ~a ~a ~a ~a ~a~n" posn span path a-color key)

           (define orig-tab (send this get-current-tab))
           (when (path? path)
             ;(printf "path: ~a ~n" path)
             (define the-tab (send this find-matching-tab path))             
             (cond [the-tab                   
                    (send this change-to-tab the-tab)]
                   [else
                    ;(printf "tab no...path-string? ~a num tabs:~a.~n" (path-string? path) (send this get-tab-count))
                    ;(send this move-current-tab-right)
                    ;(printf "tabtabtab~n")
                    ;(define new-tab (send this open-in-new-tab path)) ; this is the call that's failing
                   ; (define new-tab (send this create-new-tab))

                    ;(define new-tab (open-in-new-tab (path->string path)))
                    ;(printf "new-tab: ~a ~n" new-tab)
                    ;(send this change-to-tab new-tab)                    
                    ; Do nothing for now if the tab isn't already open
                    (printf "Failed to open tab for path: ~a~n" path)]))           

           (send (get-definitions-text) begin-edit-sequence) 
           (send (get-definitions-text) highlight-range
                 (- posn 1) (- span 1) ; otherwise we get an offset
                 a-color #:key key)           
           (send (get-definitions-text) end-edit-sequence)
             ;(send this change-to-tab orig-tab) ; return to original tab after highlighting is done
           )
         
         (define (do-forge-unhighlight key [in-all-tabs #f])
           (cond [in-all-tabs 
                  ; Unhighlight for every open tab (and return to the tab we were at when this was called)
                  (define orig-tab (send this get-current-tab))
                  (for-each (lambda (tab)
                              (send this change-to-tab tab)
                              (send (get-definitions-text) begin-edit-sequence)          
                              (send (get-definitions-text) unhighlight-ranges/key key)
                              (send (get-definitions-text) end-edit-sequence))
                            (send this get-tabs))
                  (send this change-to-tab orig-tab)]
                 [else
                  (send (get-definitions-text) begin-edit-sequence)          
                  (send (get-definitions-text) unhighlight-ranges/key key)
                  (send (get-definitions-text) end-edit-sequence)]))

         ;;;;;;;;;;;;;;;;;;;; MACHINERY TO ENABLE SHARING WITH FORGE/CORE ;;;;;;
         
         (define (setup-helper-module)
           (let* ([interactions (get-interactions-text)]
                  [link (parameterize ((current-namespace (send interactions get-user-namespace)))
                          (dynamic-require LINK-MODULE-SPEC 'link))])
             (set-box! link (vector do-forge-highlight do-forge-unhighlight))
             (set-box! local-vector (vector do-forge-highlight do-forge-unhighlight))))

         ; RackUnit extended the REPL, not the frame. 
         ; We want to do the same so that the REPL can access functionality
         ; but this particular mixin extends the unit frame, so leave a reference 
         (set-box! unit-setup-thunk setup-helper-module)))

      (define interactions-text-mixin
        (mixin ((class->interface drracket:rep:text%)) ()
          (inherit get-user-namespace)
          (super-new)
          
          (define/override (reset-console)
            (super reset-console)
            (if (unbox unit-setup-thunk)
                ((unbox unit-setup-thunk))
                (printf "Forge tool: reset-console was called without a unit-setup-thunk populated.")))))

      ; Auto-unhighlight when someone edits a tab
      (define definitions-text-mixin
        (mixin (drracket:unit:definitions-text<%> editor<%>) ()      
          (super-new)
          (define/override (on-char ev)
            (super on-char ev)
            (when (unbox local-vector)
              (let ([unhighlighter (vector-ref (unbox local-vector) 1)])
                (unhighlighter 'core))))))
      
    (drracket:get/extend:extend-interactions-text interactions-text-mixin)
    (drracket:get/extend:extend-definitions-text definitions-text-mixin)
    (drracket:get/extend:extend-unit-frame unit-mixin)))






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

    ;(drracket:get/extend:extend-unit-frame ping-button-mixin)
    ;(drracket:get/extend:extend-unit-frame mixin-run-choice)
    ;(drracket:get/extend:extend-unit-frame mixin-menu)
