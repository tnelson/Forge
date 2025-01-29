#lang racket

; With thanks to https://gist.github.com/danking/1068185

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)         
         parser-tools/yacc
         syntax/readerr
         racket/struct
         rackunit)
(provide parse-all-commands
         parse-single-command
         (struct-out policy)
         (struct-out command)
         (struct-out rule)
         (struct-out condition))

(define-tokens the-tokens (id))
(define-empty-tokens the-empty-tokens (comma not EOF pol end po do nop
                                             lparen rparen if is dot semicolon true
                                             info compare query where s a r yields
                                             permit deny))

(define-lex-abbrevs
  (identifier-characters (re-or (char-range "A" "z")
                                "-" "_"
                                (char-range "0" "9")))
  (identifier (re-+ identifier-characters))
  [lex:comment  (re-: #\/ #\/ (re-* (char-complement (re-or #\newline #\return))))])

(define policy-lexer
  (lexer-src-pos
   ("info" (token-info))
   ("compare" (token-compare))
   ("query" (token-query))
   ("where" (token-where))

   ("s" (token-s))
   ("a" (token-a))
   ("r" (token-r))

   ("permit" (token-permit))
   ("deny" (token-deny))
   
   ("." (token-dot))
   ("if:" (token-if))
   ("is" (token-is))
   ("yields" (token-yields))
   ("true" (token-true))
   ("(" (token-lparen))
   (")" (token-rparen))
   ("policy" (token-pol))
   ("end" (token-end))   
   ("po" (token-po))
   ("do" (token-do))
   ("nop" (token-nop))
   ("," (token-comma))  
   ("not" (token-not))
   (";" (token-semicolon))
   (identifier (token-id (string->symbol lexeme)))
   ; Note: without return-without-pos, the resulting position-token will be *wrapped* in another.
   ; See docs for return-without-pos
   (whitespace (return-without-pos (policy-lexer input-port)))
   ; comments
   [lex:comment (return-without-pos (policy-lexer input-port))]
   
   ((eof) (token-EOF))
   ; Custom error behavior
   (any-char (raise-read-error
              (format "Couldn't understand \"~a\"; it's not a recognized keyword or identifier.~n" lexeme)
              (object-name input-port)
              (position-line start-pos)
              (position-col start-pos)
              (position-offset start-pos)      
              (- (position-offset end-pos) (position-offset start-pos))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DESIGN NOTE
; This is technically not the "right" way to do this in Racket. 
; Instead, we should be producing syntax. However, I built on
; some prior code that did this already. Unless we need syntax
; info post-parse, keeping this.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Need these to write identically to their constructor.
; make-constructor-style-printer will not produce the correct write format.
(struct condition (sign pred args)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (lambda (obj port mode)
       (write-string (format "(condition ~a ~a ~a)" (condition-sign obj) (condition-pred obj) (condition-args obj)) port)))])   

(struct rule (decision conditions)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (lambda (obj port mode)
       (write-string (format "(rule ~a ~a)" (rule-decision obj) (rule-conditions obj)) port)))])

(struct policy (name rules)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (lambda (obj port mode)
       (write-string (format "(policy ~a ~a)" (policy-name obj) (policy-rules obj)) port)))])  
(struct command (name args)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (lambda (obj port mode)
       (write-string (format "(command ~a ~a)" (command-name obj) (command-args obj)) port)))])  

(define (general-error-message tok-ok? tok-name tok-value start-pos end-pos)
  (cond [(and tok-ok? (not (equal? tok-name 'id)))
         (format "Failed to understand command around the ~a on line ~a, column ~a.~n"
                 tok-name
                 (position-line end-pos)
                 (position-col end-pos))]
        [else
         (format "Failed to recognize word around line ~a, column ~a.~n"
                 (position-line end-pos)
                 (position-col end-pos))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In order to give better error messages, we remember each token in a command (or a malformed command)
;   and clear out the history when done or when presenting an error
; We do this by changing the lexer function that the caller gives us before sending it on to the parser.
(define token-history empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers

(define (get-reversed-tokens-so-far-no-eof)
  (filter (lambda (t) (not (equal? 'EOF t)))
          (reverse (map position-token-token token-history))))

(define (started-with token-list)  
  (define rev-tokens (get-reversed-tokens-so-far-no-eof))
  ;(printf "started-with: history:~a case:~a ~n" rev-tokens token-list)
  (cond [(>= (length rev-tokens) (length token-list))
         (define other-tokens (take rev-tokens (length token-list)))
         
         (define pairlist (map list token-list other-tokens))
         (andmap (lambda (pr) (cond [(equal? (first pr) (second pr)) #t]
                                    ; IMPORTANT: match any token of the same type (this is used for parse errors)
                                    [(and (token? (first pr))
                                          (token? (second pr))
                                          (equal? (token-name (first pr)) (token-name (second pr)))) #t]
                                    [else #f])) pairlist)]
        [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Detect invalid relation-names at the parser level
(define allowed-relations-lc
  '("admin" "accountant" "customer"
            "read" "write" "file" "under-audit"
            "owned-by" "in-training"))

(define (command-parser source-name)

  (define (validate-relation relname start-pos end-pos)
    (cond [(member (symbol->string relname) allowed-relations-lc)
           relname]
          [else           
           (set! token-history empty)
           (raise-read-error (format "Unrecognized name: ~a. In this position, valid names include: ~a." relname allowed-relations-lc)
                             source-name
                             (position-line start-pos)
                             (position-col start-pos)
                             (position-offset start-pos)      
                             (- (position-offset end-pos) (position-offset start-pos)))]))
        
  (define internal-command-parse
    (parser
   (src-pos)
   (start COMMAND)
   (end EOF semicolon)
   (error 
    (lambda (tok-ok? token-name token-value start-pos end-pos) 
      ; Token-history is reversed here.
      (printf "(DEBUG) History: ~a~n" (map position-token-token token-history))
      
      ; Can we guess at the kind of command they were trying to use?
      ; customize an error message depending on history:
      (define error-message
        (cond [(empty? token-history)
               "Please enter a command."]
              ;[(token? first-token) ; token? is not true of empty-tokens like all the correct command keywords.
              ; "Please enter a valid command."]
              
              [(started-with '(compare))
               "To compare the behavior of two policies, use 'COMPARE <policy name> <policy name> WHERE <conditions>' (without the quotes or angle-brackets). The 'WHERE' part may be omitted if there are no conditions."]
              [(started-with '(query))
               "To query the behavior of a policy, use 'QUERY <policy name> YIELDS <permit/deny> WHERE <conditions>' (without the quotes or angle-brackets)."]

              [(or (started-with `(pol ,(token-permit))) (started-with `(pol ,(token-deny))))
               "A policy must be given a name before starting to define rules. Please name the policy."]

              ; Later rules will trigger the last-resort error, which seems good enough
              [(started-with `(pol ,(token-id 'x) ,(token-id 'x)))
               "Each rule must begin with a decision: permit or deny."]                        
                                           
              ; Last resort              
              [else (general-error-message tok-ok? token-name token-value start-pos end-pos)]))
                       
      (set! token-history empty)
      (raise-read-error error-message
                        source-name
                        (position-line start-pos)
                        (position-col start-pos)
                        (position-offset start-pos)      
                        (- (position-offset end-pos) (position-offset start-pos)))))
   
   ;(debug "abac-debug.txt")
   (tokens the-tokens the-empty-tokens)
   ;(precs (left + -))
   (grammar    
    
    ; single command (may be a policy definition)
    ; if able to complete parsing one of these, clear out the token history for next attempt
    (COMMAND
     ((info)
      (begin
        (set! token-history empty)
        (command 'info empty)))
     ((compare id id)
      (begin
        (set! token-history empty)
        (command 'compare (list $2 $3))))
     ((compare id id where NONEMPTYCONDITIONLIST)
      (begin
        (set! token-history empty)
        (command 'compare (list $2 $3 $5))))
     ((query id yields DEC where NONEMPTYCONDITIONLIST)
      (begin
        (set! token-history empty)
        (command 'query (cons $2 (cons $4 $6)))))
     ((POL)
      (begin
        (set! token-history empty)
        $1))
     ; Prevent trailing whitespace and comments from clogging the parser
     (()
      (begin
        (set! token-history empty)
        '())))
    
    ; single policy definition
    (POL
     ((pol id NONEMPTYRULELIST end)
      (policy $2 $3)))
     
    (NONEMPTYRULELIST ((RULE NONEMPTYRULELIST) (cons $1 $2))
                      ((RULE) (list $1)))
    (RULE ((DEC if true dot)
           (rule $1 empty))
          ((DEC if NONEMPTYCONDITIONLIST dot)
          (rule $1 $3)))
    (NONEMPTYCONDITIONLIST ((CONDITION comma NONEMPTYCONDITIONLIST) (cons $1 $3))
                           ((CONDITION) (list $1)))

    ; Conditions will be either unary or binary
    ; "x is foo" or "x is foo-of y"
    (CONDITION ((VAR is not id)
               (condition #f (validate-relation $4 $4-start-pos $4-end-pos) (list $1)))
               ((VAR is id)                
                (condition #t (validate-relation $3 $3-start-pos $3-end-pos) (list $1)))
               ((VAR is not id VAR)
                (condition #f (validate-relation $4 $4-start-pos $4-end-pos) (list $1 $5)))
               ((VAR is id VAR)
                (condition #t (validate-relation $3 $3-start-pos $3-end-pos) (list $1 $4))))
    
    ; For now, a variable has to be either s, a, or r. No existentials!
    (VAR ((s) 's)
         ((a) 'a)
         ((r) 'r) )
    ; Likewise, only 2 decisions
    (DEC ((permit) 'permit)
         ((deny) 'deny))
    )))
  
  ; wrap the parser func. wrapper has same type
  (lambda (gen)             
    (define (new-gen) 
      (let ([token-result (gen)])
        (set! token-history (cons token-result token-history)) ; backwards. reverse later
        token-result))    
    (internal-command-parse new-gen)))

(define (get-lexer-for input)
  (lambda ()
    (port-count-lines! input)
    (policy-lexer input)))


; Prints out the tokens generated for a given input string
; Taken from Margrave

(define (pretty-position pos)
  (list (position-offset pos)
        (position-line pos)
        (position-col pos)))
(define (debug-lexer-on str)
  (printf "Debugging lexer on string: ~a:~n~n" str)
  (define pt (open-input-string str))  
  (define lex-func (get-lexer-for pt))
  
  (define (inner-func)
    (define my-token (lex-func))
    (printf "~a @ start: ~a, end: ~a. ~n~n" 
            (position-token-token my-token)
            (pretty-position (position-token-start-pos my-token))
            (pretty-position (position-token-end-pos my-token)))
    (unless (equal? 'EOF (position-token-token my-token))
      (inner-func)))
  (inner-func))

; TODO: reconstructing the parser for each input can't possibly be efficient?

; Parse entire string into a list of commands/definitions
(define (parse-all-commands src input)  
  (define cmd ((command-parser src) (get-lexer-for input)))
  ;(printf "cmd: ~a~n" cmd)
  (if (empty? cmd)
      empty        
      (cons cmd  
            (parse-all-commands src input))))

; Parse a *single* command from the port
(define (parse-single-command src input)
  ((command-parser src) (get-lexer-for input)))

(define (parser-error? x)
  (and (list? x) (not (empty? x)) (equal? (first x) 'error)))