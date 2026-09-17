#lang racket/base
;; Modelo de datos (AST) para KDL v2.0.0.
;;
;; La jerarquía es:
;;
;;   kdl-document -> (listof kdl-node)
;;   kdl-node     -> nombre + anotación + (listof kdl-argument) +
;;                   (listof kdl-property) + hijos
;;   kdl-argument -> kdl-value posicional
;;   kdl-property -> nombre + kdl-value
;;   kdl-value    -> anotación de tipo opcional + dato de Racket
;;
;; Todos los structs son #:transparent para que `equal?` compare por
;; contenido y las pruebas puedan escribir el árbol esperado literalmente.

(require racket/contract
         racket/list)

;; --------------------------------------------------------------- structs

;; Una anotación de tipo KDL — `(u8)255` — o #f si no hay.
(define annotation/c (or/c #f string?))

;; Los datos que puede llevar un valor KDL. `null` se representa con la
;; constante 'null para distinguirlo de #f (que es `#false`).
(define kdl-datum/c
  (or/c string? number? boolean? 'null))

(struct kdl-value (type datum) #:transparent)
(struct kdl-argument (value) #:transparent)
(struct kdl-property (name value) #:transparent)
(struct kdl-node (type name args props children) #:transparent)
(struct kdl-document (nodes) #:transparent)

;; ------------------------------------------------------------ predicados

;; Los contratos se definen en términos de estos predicados recursivos para
;; que un árbol mal formado se detecte en la frontera del módulo y no más
;; tarde, en medio de un recorrido.

(define (kdl-value*? v)
  (and (kdl-value? v)
       (annotation? (kdl-value-type v))
       (kdl-datum? (kdl-value-datum v))))

(define (annotation? a) (or (eq? a #f) (string? a)))

(define (kdl-datum? d)
  (or (string? d) (number? d) (boolean? d) (eq? d 'null)))

(define (kdl-argument*? a)
  (and (kdl-argument? a) (kdl-value*? (kdl-argument-value a))))

(define (kdl-property*? p)
  (and (kdl-property? p)
       (string? (kdl-property-name p))
       (kdl-value*? (kdl-property-value p))))

(define (kdl-node*? n)
  (and (kdl-node? n)
       (annotation? (kdl-node-type n))
       (string? (kdl-node-name n))
       (list? (kdl-node-args n))
       (andmap kdl-argument*? (kdl-node-args n))
       (list? (kdl-node-props n))
       (andmap kdl-property*? (kdl-node-props n))
       ;; Las claves duplicadas ya fueron resueltas por el parser.
       (let ([names (map kdl-property-name (kdl-node-props n))])
         (= (length names) (length (remove-duplicates names))))
       (list? (kdl-node-children n))
       (andmap kdl-node*? (kdl-node-children n))))

; Las claves de propiedad son únicas dentro de un nodo: el parser ya
;; resolvió los duplicados dejando la ocurrencia de más a la derecha, así que
;; un nodo con claves repetidas indica un árbol construido mal a mano.
(define (distinct-prop-names? props)
  (define names (map kdl-property-name props))
  (= (length names) (length (remove-duplicates names))))

(define props/c (and/c (listof kdl-property*?) distinct-prop-names?))

(define (kdl-document*? d)
  (and (kdl-document? d)
       (list? (kdl-document-nodes d))
       (andmap kdl-node*? (kdl-document-nodes d))))

;; ------------------------------------------------------------- utilidades

;; Busca una propiedad por nombre. Devuelve su kdl-value o `default`.
(define (kdl-node-ref node name [default #f])
  (define p (findf (λ (p) (string=? (kdl-property-name p) name))
                   (kdl-node-props node)))
  (if p (kdl-property-value p) default))

;; Los datos de los argumentos posicionales, en orden.
(define (kdl-node-arg-data node)
  (for/list ([a (in-list (kdl-node-args node))])
    (kdl-value-datum (kdl-argument-value a))))

;; ---------------------------------------------------------------- exports

(provide
 (contract-out
  [struct kdl-value ([type annotation/c] [datum kdl-datum/c])]
  [struct kdl-argument ([value kdl-value*?])]
  [struct kdl-property ([name string?] [value kdl-value*?])]
  [struct kdl-node ([type annotation/c]
                    [name string?]
                    [args (listof kdl-argument*?)]
                    [props props/c]
                    [children (listof kdl-node*?)])]
  [struct kdl-document ([nodes (listof kdl-node*?)])]
  [kdl-node-ref (->* (kdl-node*? string?) (any/c) any/c)]
  [kdl-node-arg-data (-> kdl-node*? (listof kdl-datum/c))]
  [kdl-document*? (-> any/c boolean?)]
  [kdl-node*? (-> any/c boolean?)]
  [kdl-value*? (-> any/c boolean?)])
 annotation/c
 kdl-datum/c)
