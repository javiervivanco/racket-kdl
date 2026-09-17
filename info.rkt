#lang info
(define collection "kdl")
(define deps '("base" "parser-tools-lib"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/kdl.scrbl" ())))
(define pkg-desc "Lexer, parser, AST and jsexpr-like data for the KDL Document Language v2.0.0")
(define version "1.0")
(define pkg-authors '("javier123mendoza@gmail.com"))
(define license 'MIT)
