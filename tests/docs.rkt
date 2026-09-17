#lang racket/base
;; Los ejemplos de la documentación son ejecutables: construir el documento
;; evalúa cada bloque `@examples`, y un `eval:error` falla si la expresión
;; NO levanta el error prometido. Esta prueba fuerza esa construcción, de
;; modo que la documentación no pueda quedar desincronizada del código.

(require rackunit racket/runtime-path)

(define-runtime-path scrbl "../scribblings/kdl.scrbl")

(test-case "los ejemplos de la documentación evalúan como dice el texto"
  (check-not-exn (λ () (dynamic-require scrbl 'doc))))
