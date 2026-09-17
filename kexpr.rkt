#lang racket/base
;; Conversión entre KDL y kexpr.
;;
;; Un kexpr es a KDL lo que un jsexpr es a JSON: datos planos de Racket, sin
;; structs ni contratos que respetar, pensados para recorrer con `hash-ref` y
;; `for`. Un documento es una lista de nodos, y cada nodo es un hash:
;;
;;   knode  ::= (hasheq 'name     symbol?
;;                      'type     symbol?          ; sólo si hay anotación
;;                      'args     (listof kvalue)
;;                      'props    (hasheq symbol? kvalue)
;;                      'children (listof knode))
;;
;;   kvalue ::= string? | number? | boolean? | 'null
;;            | (hasheq 'type symbol? 'value kvalue)   ; valor anotado
;;
;; Los nombres —del nodo, de las propiedades y de las anotaciones de tipo— son
;; símbolos, igual que las claves de un jsexpr; los strings quedan para los
;; datos. Nótese que un valor sin comillas y uno entrecomillado son el mismo
;; string: KDL los considera equivalentes, así que la distinción no sobrevive
;; —ni debe— hasta el kexpr.
;;
;; La conversión no pierde nada: los tipos nativos de KDL (números, booleanos,
;; null) y las anotaciones de tipo sobreviven el viaje de ida y vuelta.
;;
;; `kdl->kexpr` siempre emite las cuatro claves de un nodo, para que se puedan
;; leer sin default. `kexpr->kdl` acepta que falten las vacías.

(require racket/string
         racket/list
         racket/match
         racket/contract
         "ast.rkt"
         "parser.rkt"
         "lexer.rkt")

(provide (contract-out
          [kdl->kexpr (-> (or/c string? input-port?) (listof hash?))]
          [kexpr->kdl (-> (or/c hash? (listof hash?)) string?)]))

;; =========================================================== KDL -> kexpr

(define (kdl->kexpr in)
  (for/list ([n (in-list (kdl-document-nodes (parse-kdl in)))])
    (node->kexpr n)))

(define (node->kexpr n)
  (define base
    (hasheq 'name     (string->symbol (kdl-node-name n))
            'args     (map (λ (a) (value->kdatum (kdl-argument-value a)))
                           (kdl-node-args n))
            'props    (for/hasheq ([p (in-list (kdl-node-props n))])
                        (values (string->symbol (kdl-property-name p))
                                (value->kdatum (kdl-property-value p))))
            'children (map node->kexpr (kdl-node-children n))))
  (if (kdl-node-type n)
      (hash-set base 'type (string->symbol (kdl-node-type n)))
      base))

;; Un valor sin anotación es el dato pelado; con anotación se envuelve, que es
;; la única forma de no perderla.
(define (value->kdatum v)
  (define d (kdl-value-datum v))
  (if (kdl-value-type v)
      (hasheq 'type (string->symbol (kdl-value-type v)) 'value d)
      d))

;; =========================================================== kexpr -> KDL

(define (kexpr->kdl k)
  (define nodes (if (hash? k) (list k) k))
  (string-join (for/list ([n (in-list nodes)]) (node->string n 0)) "\n"))

(define (node->string n depth)
  (define pad (make-string (* 4 depth) #\space))
  (define name (name->string (hash-ref n 'name (λ () (kexpr-error "un nodo necesita 'name")))
                             "'name"))
  (define parts
    (append
     (list (string-append (type-prefix (hash-ref n 'type #f)) (ident->string name)))
     (for/list ([a (in-list (hash-ref n 'args '()))]) (kdatum->string a))
     ;; El orden de un hash no está definido, así que se ordena por clave para
     ;; que la salida sea reproducible.
     (for/list ([key (in-list (sort (hash-keys (hash-ref n 'props (hasheq)))
                                    symbol<?))])
       (format "~a=~a"
               (ident->string (symbol->string key))
               (kdatum->string (hash-ref (hash-ref n 'props) key))))))
  (define head (string-append pad (string-join parts " ")))
  (define kids (hash-ref n 'children '()))
  (if (null? kids)
      head
      (string-append head " {\n"
                     (string-join (for/list ([c (in-list kids)])
                                    (node->string c (add1 depth)))
                                  "\n")
                     "\n" pad "}")))

(define (type-prefix t)
  (if t (format "(~a)" (ident->string (name->string t "'type"))) ""))

;; Un nombre se escribe como símbolo, pero también se acepta un string: al
;; leer se es estricto, al escribir conviene ser tolerante.
(define (name->string v what)
  (cond [(symbol? v) (symbol->string v)]
        [(string? v) v]
        [else (kexpr-error "~a debe ser un símbolo o un string: ~s" what v)]))

(define (kdatum->string d)
  (match d
    [(? hash?)
     (string-append (type-prefix (hash-ref d 'type #f))
                    (kdatum->string
                     (hash-ref d 'value
                               (λ () (kexpr-error "un valor anotado necesita 'value")))))]
    [(? string?) (ident->string d)]
    [#t "#true"]
    [#f "#false"]
    ['null "#null"]
    [(? number?) (number->kdl d)]
    [_ (kexpr-error "valor no representable en KDL: ~s" d)]))

;; Un string se escribe sin comillas sólo si es un identificador legal; si no,
;; se cita y se escapa.
(define (ident->string s)
  (if (valid-identifier? s) s (quote-string s)))

(define (quote-string s)
  (define out (open-output-string))
  (write-char #\" out)
  (for ([c (in-string s)])
    (define i (char->integer c))
    (cond
      [(char=? c #\") (write-string "\\\"" out)]
      [(char=? c #\\) (write-string "\\\\" out)]
      [(char=? c #\newline) (write-string "\\n" out)]
      [(char=? c #\return) (write-string "\\r" out)]
      [(char=? c #\tab) (write-string "\\t" out)]
      [(char=? c #\backspace) (write-string "\\b" out)]
      [(char=? c #\page) (write-string "\\f" out)]
      ;; Los code points que no pueden aparecer literalmente se emiten como
      ;; escape, que sí es legal.
      [(or (< i #x20) (= i #x7F) (disallowed-literal? c))
       (write-string (format "\\u{~a}" (number->string i 16)) out)]
      [else (write-char c out)]))
  (write-char #\" out)
  (get-output-string out))

(define (number->kdl n)
  (cond
    [(and (rational? n) (exact? n) (integer? n)) (number->string n)]
    [(eqv? n +inf.0) "#inf"]
    [(eqv? n -inf.0) "#-inf"]
    [(eqv? n +nan.0) "#nan"]
    [(and (real? n) (exact? n)) (number->string (exact->inexact n))]
    [(real? n) (number->string n)]
    [else (kexpr-error "KDL no representa este número: ~s" n)]))

(define (kexpr-error fmt . args)
  (raise (exn:fail:kdl (format "kexpr->kdl: ~a" (apply format fmt args))
                       (current-continuation-marks)
                       #f #f)))
