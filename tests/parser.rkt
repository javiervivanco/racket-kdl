#lang racket/base
;; Suite de pruebas del lexer + parser de KDL v2.0.0.

(require rackunit
         racket/list
         racket/string
         racket/math
         kdl)

;; Atajos para escribir el árbol esperado sin ruido.
(define (v d [t #f]) (kdl-value t d))
(define (a d [t #f]) (kdl-argument (kdl-value t d)))
(define (p n d [t #f]) (kdl-property n (kdl-value t d)))
(define (n name #:type [type #f] #:args [args '()]
              #:props [props '()] #:kids [kids '()])
  (kdl-node type name args props kids))

(define (parse1 s) (parse-kdl-node s))
(define (docs s) (kdl-document-nodes (parse-kdl s)))

;; =====================================================================
;; Documento representativo: reúne todos los edge-cases en una sola
;; cadena y se valida la forma final del S-expression generado.
;; =====================================================================

(define representative #<<KDL
// Configuración de ejemplo
(app)servidor "web" puerto=8080 activo=#true {
    // anotaciones de tipo en nodo, argumento y propiedad
    (date)desplegado "2024-01-15" por=(email)"ops@ejemplo.com"

    // continuación de línea: el nodo sigue en la línea siguiente
    limites \
        memoria=512 \
        cpu=2

    // strings crudos y multilínea
    patron #"^\d+\.\d+$"#
    doble ##"lleva "# adentro"##
    texto """
        primera
          sangrada

        última
        """

    /* comentario de bloque /* anidado */ que se descarta */

    // slashdash en todas sus formas
    flags /-descartado "conservado" /-clave="x" real="y"
    /-nodo-entero 1 2 { hijo profundo; }
    hijos { uno; dos; } /-{ estos otros se descartan }

    // la última clave gana
    repetida x=1 x=2 x=3

    // números en todas las bases
    numeros 0xff 0o17 0b1010 1_000_000 -2.5e3 #inf #-inf
}
KDL
  )

(define doc (parse-kdl representative))
(check-pred kdl-document? doc)
(check-equal? (length (kdl-document-nodes doc)) 1)

(define srv (car (kdl-document-nodes doc)))

(test-case "nodo raíz: anotación, nombre, argumento y propiedades"
  (check-equal? (kdl-node-type srv) "app")
  (check-equal? (kdl-node-name srv) "servidor")
  (check-equal? (kdl-node-arg-data srv) '("web"))
  (check-equal? (kdl-value-datum (kdl-node-ref srv "puerto")) 8080)
  (check-equal? (kdl-value-datum (kdl-node-ref srv "activo")) #t))

(define kids (kdl-node-children srv))
(define (kid name) (findf (λ (k) (string=? (kdl-node-name k) name)) kids))

(test-case "anotaciones de tipo en argumento y propiedad"
  (define d (kid "desplegado"))
  (check-equal? (kdl-node-type d) "date")
  (check-equal? (kdl-argument-value (car (kdl-node-args d)))
                (v "2024-01-15"))
  (check-equal? (kdl-node-ref d "por") (v "ops@ejemplo.com" "email")))

(test-case "continuación de línea"
  (check-equal? (kid "limites")
                (n "limites" #:props (list (p "memoria" 512) (p "cpu" 2)))))

(test-case "raw strings"
  ;; Dentro de un raw string no hay escapes: la barra es una barra.
  (check-equal? (kdl-node-arg-data (kid "patron")) (list "^\\d+\\.\\d+$"))
  ;; Con `##` se puede incluir la secuencia `"#` literalmente.
  (check-equal? (kdl-node-arg-data (kid "doble")) (list "lleva \"# adentro")))

(test-case "multi-line string: dedent según la línea de cierre"
  (check-equal? (kdl-node-arg-data (kid "texto"))
                (list "primera\n  sangrada\n\núltima")))

(test-case "slashdash sobre argumentos y propiedades"
  (check-equal? (kid "flags")
                (n "flags" #:args (list (a "conservado"))
                          #:props (list (p "real" "y")))))

(test-case "slashdash sobre un nodo entero y sobre un bloque de hijos"
  ;; Ni el nodo comentado ni el segundo bloque aparecen en el árbol.
  (check-false (kid "nodo-entero"))
  (check-equal? (map kdl-node-name kids)
                '("desplegado" "limites" "patron" "doble" "texto"
                  "flags" "hijos" "repetida" "numeros"))
  (check-equal? (map kdl-node-name (kdl-node-children (kid "hijos")))
                '("uno" "dos")))

(test-case "claves duplicadas: gana la de más a la derecha"
  (check-equal? (kid "repetida") (n "repetida" #:props (list (p "x" 3)))))

(test-case "números en todas las bases"
  (check-equal? (kdl-node-arg-data (kid "numeros"))
                (list 255 15 10 1000000 -2500.0 +inf.0 -inf.0)))

;; =====================================================================
;; Casos puntuales
;; =====================================================================

(test-case "documento vacío y sólo comentarios"
  (check-equal? (docs "") '())
  (check-equal? (docs "   \n // nada \n /* tampoco */ \n") '()))

(test-case "terminadores de nodo"
  (check-equal? (map kdl-node-name (docs "a; b\nc // fin\nd")) '("a" "b" "c" "d"))
  (check-equal? (map kdl-node-name (docs "p { x; y }")) '("p")))

(test-case "identificadores sin comillas"
  (check-equal? (kdl-node-name (parse1 "foo-bar_baz.qux")) "foo-bar_baz.qux")
  (check-equal? (kdl-node-name (parse1 "-")) "-")
  (check-equal? (kdl-node-name (parse1 ".algo")) ".algo")
  ;; Un identificador no puede empezar como número.
  (check-exn exn:fail:kdl? (λ () (parse1 "1abc")))
  (check-exn exn:fail:kdl? (λ () (parse1 "-1abc")))
  ;; En v2 las palabras sueltas son error: hay que escribir #true.
  (check-exn exn:fail:kdl? (λ () (parse1 "a true")))
  (check-exn exn:fail:kdl? (λ () (parse1 "a null"))))

(test-case "keywords"
  (check-equal? (kdl-node-arg-data (parse1 "k #true #false #null"))
                (list #t #f 'null))
  (check-true (nan? (car (kdl-node-arg-data (parse1 "k #nan"))))))

(test-case "escapes en strings con comillas"
  (check-equal? (kdl-node-arg-data (parse1 "k \"a\\tb\\nc\""))
                (list "a\tb\nc"))
  (check-equal? (kdl-node-arg-data (parse1 "k \"\\u{1F600}\"")) (list "😀"))
  ;; \s es un espacio; el escape de whitespace se traga el salto de línea.
  (check-equal? (kdl-node-arg-data (parse1 "k \"a\\sb\"")) (list "a b"))
  (check-equal? (kdl-node-arg-data (parse1 "k \"uno\\\n     dos\""))
                (list "unodos"))
  (check-exn exn:fail:kdl? (λ () (parse1 "k \"escape \\q malo\"")))
  (check-exn exn:fail:kdl? (λ () (parse1 "k \"sin cerrar"))))

(test-case "el espacio alrededor del = es legal"
  (check-equal? (parse1 "a x = 1") (n "a" #:props (list (p "x" 1)))))

(test-case "anotación de tipo sobre el nombre del nodo"
  (check-equal? (parse1 "(t)nodo") (n "nodo" #:type "t"))
  (check-exn exn:fail:kdl? (λ () (parse1 "(t nodo"))))

(test-case "errores de estructura"
  (check-exn exn:fail:kdl? (λ () (parse-kdl "a { b")))
  (check-exn exn:fail:kdl? (λ () (parse-kdl "a }")))
  (check-exn exn:fail:kdl? (λ () (parse-kdl "1 2 3")))
  (check-exn exn:fail:kdl? (λ () (parse-kdl "a {} {}")))
  ;; Un `/-` no vuelve válido a un nodo mal formado: igual se analiza.
  (check-exn exn:fail:kdl? (λ () (parse-kdl "/-a { b"))))

(test-case "el error trae línea y columna"
  (define e (with-handlers ([exn:fail:kdl? values])
              (parse-kdl "ok\nmal \"sin cerrar\n")))
  (check-equal? (exn:fail:kdl-line e) 2))

(test-case "multi-line string: sangrado inconsistente es error"
  (check-exn exn:fail:kdl?
             (λ () (parse1 "k \"\"\"\n  hola\n    mundo\n      \"\"\"")))
  ;; Debe abrir con un salto de línea.
  (check-exn exn:fail:kdl? (λ () (parse1 "k \"\"\"hola\n\"\"\""))))

(test-case "multi-line raw string"
  (check-equal? (kdl-node-arg-data (parse1 "k #\"\"\"\n  a\\nb\n  \"\"\"#"))
                (list "a\\nb")))

(test-case "los contratos del AST rechazan árboles mal formados"
  (check-exn exn:fail:contract? (λ () (kdl-value #f (list 1 2))))
  (check-exn exn:fail:contract? (λ () (kdl-node #f 'no-string '() '() '())))
  (check-exn exn:fail:contract?
             (λ () (kdl-node #f "a" (list (a 1)) (list (p "x" 1) (p "x" 2)) '()))))

(test-case "utilidades del AST"
  (define node (parse1 "a 1 2 x=\"v\""))
  (check-equal? (kdl-node-arg-data node) '(1 2))
  (check-equal? (kdl-node-ref node "x") (v "v"))
  (check-equal? (kdl-node-ref node "falta" 'nope) 'nope))

;; =====================================================================
;; Casos derivados de la suite oficial de conformidad (kdl-org/kdl).
;; =====================================================================

(test-case "hace falta un espacio antes de cada entry"
  (check-exn exn:fail:kdl? (λ () (parse1 "node\"arg\"")))
  (check-exn exn:fail:kdl? (λ () (parse1 "node foo=\"v\"bar=5")))
  (check-exn exn:fail:kdl? (λ () (parse1 "node 1\"dos\"")))
  ;; Los raw strings de KDL v1 (`r"..."`) quedan descartados por esta regla.
  (check-exn exn:fail:kdl? (λ () (parse1 "node r\"foo\"")))
  ;; Un identificador no puede llevar paréntesis: `(bar)` abre una anotación.
  (check-exn exn:fail:kdl? (λ () (parse1 "foo123(bar)foo weeee")))
  ;; Los hijos sí pueden ir pegados al nombre.
  (check-equal? (map kdl-node-name (kdl-node-children (parse1 "node{hijo;}")))
                '("hijo")))

(test-case "el vertical tab termina el nodo"
  ;; KDL clasifica U+000B como salto de línea, no como espacio.
  (check-equal? (map kdl-node-name (docs "node arg\u000Bnode2 arg2"))
                '("node" "node2")))

(test-case "code points prohibidos como literales"
  (check-exn exn:fail:kdl? (λ () (parse1 "node \"\u007F\"")))     ; DELETE
  (check-exn exn:fail:kdl? (λ () (parse1 "node \"\u200E\"")))     ; LRM
  (check-exn exn:fail:kdl? (λ () (parse1 "node \"\u202A\"")))     ; LRE
  (check-exn exn:fail:kdl? (λ () (parse1 "node\u0001 arg")))      ; control C0
  ;; Escritos como escape sí son válidos.
  (check-equal? (kdl-node-arg-data (parse1 "node \"\\u{200E}\"")) (list "\u200E"))
  ;; El BOM sólo puede estar al principio del documento.
  (check-equal? (kdl-node-name (parse1 "\uFEFFnode")) "node")
  (check-exn exn:fail:kdl? (λ () (parse1 "node \uFEFF1"))))

(test-case "límites de los escapes unicode"
  (check-exn exn:fail:kdl? (λ () (parse1 "node \"\\u{0012345}\"")))  ; 7 dígitos
  (check-exn exn:fail:kdl? (λ () (parse1 "node \"\\u{}\"")))
  (check-exn exn:fail:kdl? (λ () (parse1 "node \"\\u{D800}\"")))     ; surrogate
  (check-equal? (kdl-node-arg-data (parse1 "node \"\\u{10FFFF}\""))
                (list (string (integer->char #x10FFFF)))))

(test-case "un raw string de una línea no admite saltos de línea"
  (check-exn exn:fail:kdl? (λ () (parse1 "node #\"\nhola\n\"#"))))

(test-case "los hijos comentados van después de los entries"
  (check-exn exn:fail:kdl? (λ () (parse1 "node /-{ a } foo { b }")))
  ;; Después del bloque real sí se admiten.
  (check-equal? (map kdl-node-name (kdl-node-children (parse1 "node { a } /-{ b }")))
                '("a")))
