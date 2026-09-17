#lang racket/base
;; Parser recursivo descendente para KDL v2.0.0.
;;
;; KDL es sensible al espacio en blanco: el espacio separa entries, el salto
;; de línea termina nodos y una continuación de línea los extiende. Eso se
;; expresa con torpeza en una gramática LALR, así que en lugar de yacc el
;; parser recorre la lista de tokens con `match`, que además deja el
;; tratamiento del slashdash a la vista en un solo lugar.
;;
;; El slashdash (`/-`) es un comentario *semántico*: el elemento que sigue se
;; analiza igual que cualquier otro — tiene que ser sintácticamente válido —
;; y recién después se descarta. Por eso el parser nunca lo saltea a nivel
;; léxico: llama a la misma función que usaría y tira el resultado.

(require racket/match
         racket/list
         racket/contract
         parser-tools/lex
         "ast.rkt"
         "lexer.rkt")

(provide (contract-out
          [parse-kdl (-> (or/c string? input-port?) kdl-document*?)]
          [parse-kdl-node (-> (or/c string? input-port?) kdl-node*?)]))

;; ----------------------------------------------------------- cursor

;; Un cursor sobre el vector de tokens: `pos` es un box con el índice actual.
(struct cursor (toks pos) #:transparent)

(define (make-cursor in)
  (cursor (list->vector (lex-kdl in)) (box 0)))

(define (peek-tok c [offset 0])
  (define v (cursor-toks c))
  (define i (min (+ (unbox (cursor-pos c)) offset) (sub1 (vector-length v))))
  (vector-ref v i))

;; El nombre del token actual: 'STRING, 'WS, 'LBRACE, etc.
(define (peek-name c [offset 0])
  (token-name (position-token-token (peek-tok c offset))))

(define (peek-value c [offset 0])
  (token-value (position-token-token (peek-tok c offset))))

(define (advance! c)
  (define t (peek-tok c))
  (when (< (unbox (cursor-pos c)) (sub1 (vector-length (cursor-toks c))))
    (set-box! (cursor-pos c) (add1 (unbox (cursor-pos c)))))
  t)

(define (at-eof? c) (eq? (peek-name c) 'EOF))

;; Posición del token actual, para los mensajes de error.
(define (here c)
  (define p (position-token-start-pos (peek-tok c)))
  (values (position-line p) (position-col p)))

(define (parse-error c fmt . args)
  (define-values (line col) (here c))
  (apply kdl-error line col fmt args))

;; ------------------------------------------------------- espacios

;; node-space: separa entries dentro de un nodo. El lexer ya convirtió las
;; continuaciones de línea y los comentarios de bloque en WS.
(define (skip-node-space! c)
  (let loop () (when (eq? (peek-name c) 'WS) (advance! c) (loop))))

;; line-space: además de node-space, admite saltos de línea. Es lo que
;; separa nodos entre sí.
(define (skip-line-space! c)
  (let loop ()
    (when (memq (peek-name c) '(WS NEWLINE))
      (advance! c)
      (loop))))

;; --------------------------------------------------------- documento

(define (parse-kdl in)
  (define c (make-cursor in))
  (define nodes (parse-nodes c))
  (skip-line-space! c)
  (unless (at-eof? c)
    (parse-error c "se esperaba el fin del documento, apareció ~a" (peek-name c)))
  (kdl-document nodes))

;; Atajo para las pruebas y para quien sabe que el documento tiene un nodo.
(define (parse-kdl-node in)
  (match (kdl-document-nodes (parse-kdl in))
    [(list n) n]
    [ns (error 'parse-kdl-node "se esperaba un único nodo, hay ~a" (length ns))]))

;; nodes := (line-space* node)* line-space*
;; Se detiene en `}` (fin de un bloque de hijos) o en el fin del documento.
(define (parse-nodes c)
  (let loop ([acc '()])
    (skip-line-space! c)
    (cond
      [(at-eof? c) (reverse acc)]
      [(eq? (peek-name c) 'RBRACE) (reverse acc)]
      [else
       (match (parse-node c)
         ['skipped (loop acc)]
         [n (loop (cons n acc))])])))

;; ------------------------------------------------------------- nodo

;; Devuelve un kdl-node, o 'skipped si el nodo venía precedido de `/-`.
;; El nodo comentado se analiza completo igual: un `/-` no vuelve válido a
;; un nodo mal formado.
(define (parse-node c)
  (cond
    [(eq? (peek-name c) 'SLASHDASH)
     (advance! c)
     (skip-line-space! c)
     (parse-node c)          ; se consume entero...
     'skipped]               ; ...y se descarta
    [else (parse-node-body c)]))

(define (parse-node-body c)
  (define type (and (eq? (peek-name c) 'LPAREN) (parse-type c)))
  (skip-node-space! c)
  (define name
    (match (peek-name c)
      ['STRING (token-value (position-token-token (advance! c)))]
      [_ (parse-error c "se esperaba el nombre de un nodo")]))
  (define-values (args props children) (parse-node-entries c))
  (kdl-node type name args (dedupe-props props) children))

;; Recorre entries e hijos hasta el terminador del nodo.
(define (parse-node-entries c)
  ;; `sep?` recuerda si hubo espacio antes del token actual: la gramática
  ;; exige un separador delante de cada entry (`node"arg"` es un error, y es
  ;; lo que descarta de paso los raw strings de KDL v1, `r"..."`). Los hijos,
  ;; en cambio, pueden ir pegados: `node{ ... }` es válido.
  ;; `seen-children?` recuerda si ya apareció un bloque de hijos —aunque haya
  ;; sido comentado con `/-`—, porque después de él el nodo debe terminar.
  (let loop ([args '()] [props '()] [children #f] [seen-children? #f])
    (define sep? (eq? (peek-name c) 'WS))
    (skip-node-space! c)
    (match (peek-name c)
      ;; node-terminator := newline | ';' | '}' | eof
      [(or 'NEWLINE 'SEMI)
       (advance! c)
       (values (reverse args) (reverse props) (or children '()))]
      [(or 'RBRACE 'EOF)
       (values (reverse args) (reverse props) (or children '()))]

      ['SLASHDASH
       (advance! c)
       (skip-line-space! c)
       ;; `/-` puede comentar un entry o el bloque de hijos entero; en ambos
       ;; casos se analiza igual y recién después se descarta.
       (cond
         [(eq? (peek-name c) 'LBRACE)
          (parse-children c)
          (loop args props children #t)]
         [else
          (when seen-children?
            (parse-error c "después del bloque de hijos el nodo debe terminar"))
          (parse-entry c)
          (loop args props children seen-children?)])]

      ['LBRACE
       (when children
         (parse-error c "un nodo no puede tener dos bloques de hijos"))
       (loop args props (parse-children c) #t)]

      [_
       (when seen-children?
         (parse-error c "después del bloque de hijos el nodo debe terminar"))
       (unless sep?
         (parse-error c "falta un espacio antes de este argumento o propiedad"))
       (match (parse-entry c)
         [(? kdl-property? p) (loop args (cons p props) children seen-children?)]
         [(? kdl-argument? a) (loop (cons a args) props children seen-children?)])])))

(define (parse-children c)
  (advance! c)                                  ; consume '{'
  (define nodes (parse-nodes c))
  (skip-line-space! c)
  (unless (eq? (peek-name c) 'RBRACE)
    (parse-error c "falta `}` para cerrar el bloque de hijos"))
  (advance! c)
  nodes)

;; ---------------------------------------------------------- entries

;; node-prop-or-arg := prop | value
;; Se distingue mirando si tras el string viene un `=`, que puede estar
;; separado por espacios (`prop := string node-space* '=' node-space* value`).
(define (parse-entry c)
  (cond
    [(and (eq? (peek-name c) 'STRING) (eq-follows? c))
     (define name (token-value (position-token-token (advance! c))))
     (skip-node-space! c)
     (advance! c)                               ; consume '='
     (skip-node-space! c)
     (kdl-property name (parse-value c))]
    [else (kdl-argument (parse-value c))]))

;; ¿Después del string actual (salteando espacios) viene un `=`?
(define (eq-follows? c)
  (let loop ([i 1])
    (match (peek-name c i)
      ['WS (loop (add1 i))]
      ['EQ #t]
      [_ #f])))

;; value := type? node-space* (string | number | keyword)
(define (parse-value c)
  (define type (and (eq? (peek-name c) 'LPAREN) (parse-type c)))
  (skip-node-space! c)
  (match (peek-name c)
    ['STRING  (kdl-value type (token-value (position-token-token (advance! c))))]
    ['NUMBER  (kdl-value type (token-value (position-token-token (advance! c))))]
    ['KEYWORD (kdl-value type (token-value (position-token-token (advance! c))))]
    [_ (parse-error c "se esperaba un valor")]))

;; type := '(' node-space* string node-space* ')'
(define (parse-type c)
  (advance! c)                                  ; consume '('
  (skip-node-space! c)
  (define name
    (match (peek-name c)
      ['STRING (token-value (position-token-token (advance! c)))]
      [_ (parse-error c "una anotación de tipo debe contener un string")]))
  (skip-node-space! c)
  (unless (eq? (peek-name c) 'RPAREN)
    (parse-error c "falta `)` para cerrar la anotación de tipo"))
  (advance! c)
  name)

;; ------------------------------------------------------ propiedades

;; Las propiedades se procesan de izquierda a derecha y la última ocurrencia
;; de una clave gana, conservando su posición: `a x=1 x=2` equivale a `a x=2`.
(define (dedupe-props props)
  (for/list ([p (in-list props)]
             [i (in-naturals)]
             #:unless (for/or ([q (in-list (drop props (add1 i)))])
                        (string=? (kdl-property-name q) (kdl-property-name p))))
    p))
