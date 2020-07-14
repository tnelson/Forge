#lang racket

(provide upload-file)

; upload-file :: string ->
(define (upload-file file-path)
    (define username "alice")
    (define personal-token "fs52knf535djbfk2je43b2436")
    (define id (github-identity 'personal-token (list username personal-token)))
     
    (define github (github-api id))
    (github "/users/plt/repos"))


