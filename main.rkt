#lang racket/base
;; kdl — lexer, parser, AST y conversión a kexpr para KDL v2.0.0,
;; en Racket puro.
;;
;;   (parse-kdl "node 1 k=\"v\" { child; }")   ; AST con structs
;;   (kdl->kexpr "node 1 k=\"v\"")             ; datos planos, estilo jsexpr
;;   (kexpr->kdl (list (hasheq 'name "node"))) ; y de vuelta a texto

(require "ast.rkt" "parser.rkt" "lexer.rkt" "kexpr.rkt")

(provide (all-from-out "ast.rkt")
         (all-from-out "parser.rkt")
         (all-from-out "kexpr.rkt")
         (struct-out exn:fail:kdl))
