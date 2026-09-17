#lang racket/base
;; Lexer para KDL v2.0.0 (parser-tools/lex).
;;
;; Tres construcciones de KDL no se pueden expresar con una expresión regular,
;; así que sus reglas delegan en una función que sigue leyendo del puerto:
;;
;;   * raw strings, cuyo delimitador lleva una cantidad arbitraria de `#`
;;     que debe reaparecer al cerrar (haría falta una retrorreferencia);
;;   * comentarios `/* */`, que anidan;
;;   * multi-line strings, cuyo valor depende del sangrado de la línea de
;;     cierre.
;;
;; El lexer resuelve además dos detalles que le simplifican la vida al parser:
;; una continuación de línea (`\` al final de la línea) se emite como espacio,
;; y un comentario `//` se emite como salto de línea, que es exactamente su
;; papel en la gramática (`node-terminator := single-line-comment | newline |
;; ';' | eof`).

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/string
         racket/list
         racket/port)

(provide lex-kdl
         (struct-out exn:fail:kdl)
         kdl-error
         (all-defined-out))

;; --------------------------------------------------------------- errores

(struct exn:fail:kdl exn:fail (line col) #:transparent)

(define (kdl-error line col fmt . args)
  (raise (exn:fail:kdl
          (format "kdl: ~a~a"
                  (apply format fmt args)
                  (if line (format " (línea ~a, columna ~a)" line (add1 col)) ""))
          (current-continuation-marks)
          line col)))

;; ---------------------------------------------------------------- tokens

(define-tokens kdl-tokens (STRING NUMBER KEYWORD))
(define-empty-tokens kdl-empty-tokens
  (LBRACE RBRACE LPAREN RPAREN EQ SEMI NEWLINE WS SLASHDASH EOF))

;; ------------------------------------------------------- clases de chars

;; La tabla de la spec: los White_Space que no son salto de línea.
(define-lex-abbrev unicode-space
  (:or #\tab #\space
       #\u00A0 #\u1680
       (char-range #\u2000 #\u200A)
       #\u202F #\u205F #\u3000))

;; La tabla de la spec: los White_Space que sí son salto de línea.
(define-lex-abbrev newline-char
  (:or #\return #\newline #\vtab #\page #\u0085
       #\u2028 #\u2029))

;; identifier-char := unicode - unicode-space - newline - [\/(){};[]"#=]
(define-lex-abbrev ident-char
  (char-complement
   (:or unicode-space newline-char
        #\\ #\/ #\( #\) #\{ #\} #\; #\[ #\] #\" #\# #\=)))

(define-lex-abbrev digit (char-range #\0 #\9))
(define-lex-abbrev sign (:or #\+ #\-))
(define-lex-abbrev hex-digit
  (:or digit (char-range #\a #\f) (char-range #\A #\F)))
(define-lex-abbrev integer-body (:: digit (:* (:or digit #\_))))
(define-lex-abbrev exponent (:: (:or #\e #\E) (:? sign) integer-body))

(define-lex-abbrev decimal-num
  (:: (:? sign) integer-body (:? (:: #\. integer-body)) (:? exponent)))
(define-lex-abbrev hex-num
  (:: (:? sign) "0x" hex-digit (:* (:or hex-digit #\_))))
(define-lex-abbrev octal-num
  (:: (:? sign) "0o" (char-range #\0 #\7) (:* (:or (char-range #\0 #\7) #\_))))
(define-lex-abbrev binary-num
  (:: (:? sign) "0b" (:or #\0 #\1) (:* (:or #\0 #\1 #\_))))

;; -------------------------------------------------------- identificadores

;; Palabras que sólo son válidas con `#`; sueltas son un error, para que
;; `activo true` no se lea en silencio como el string "true".
(define reserved-words '("true" "false" "null" "inf" "-inf" "nan"))

(define (digit-char? c) (and c (char<=? #\0 c #\9)))
(define (sign-char? c) (and c (or (char=? c #\+) (char=? c #\-))))

;; La gramática admite tres formas de identificador sin comillas
;; (unambiguous, signed y dotted); las tres existen para garantizar que
;; ningún identificador pueda confundirse con el comienzo de un número.
;; identifier-char := unicode - unicode-space - newline - [\/(){};[]"#=]
;;                    - disallowed-literal-code-points
(define (identifier-char? c)
  (not (or (space-char? c)
           (newline-char? c)
           (memv c (list #\\ #\/ #\( #\) #\{ #\} #\; #\[ #\] #\" #\# #\=))
           (disallowed-literal? c))))

(define (valid-identifier? s)
  (define n (string-length s))
  (define (ref i) (and (< i n) (string-ref s i)))
  (cond
    [(zero? n) #f]
    [(member s reserved-words) #f]
    ;; El lexer ya sólo junta identifier-chars, pero esta función también la
    ;; usa el escritor para decidir si un string puede ir sin comillas.
    [(not (for/and ([c (in-string s)]) (identifier-char? c))) #f]
    [(digit-char? (ref 0)) #f]                     ; arrancaría como número
    [(sign-char? (ref 0))                          ; signed-ident
     (cond [(= n 1) #t]
           [(char=? (ref 1) #\.) (not (digit-char? (ref 2)))]
           [else (not (digit-char? (ref 1)))])]
    [(char=? (ref 0) #\.)                          ; dotted-ident
     (not (digit-char? (ref 1)))]
    [else #t]))                                    ; unambiguous-ident

;; ---------------------------------------------------------------- números

;; Convierte el lexema a un número de Racket. Los `_` son separadores; las
;; bases explícitas usan la conversión por radix de Racket.
(define (lexeme->number s line col)
  (define t (string-replace s "_" ""))
  (define neg? (char=? (string-ref t 0) #\-))
  (define unsigned (if (sign-char? (string-ref t 0)) (substring t 1) t))
  (define (parse-radix body radix)
    (define v (string->number body radix))
    (unless v (kdl-error line col "número inválido: ~a" s))
    (if neg? (- v) v))
  (cond
    [(regexp-match? #rx"^0[xX]" unsigned) (parse-radix (substring unsigned 2) 16)]
    [(regexp-match? #rx"^0[oO]" unsigned) (parse-radix (substring unsigned 2) 8)]
    [(regexp-match? #rx"^0[bB]" unsigned) (parse-radix (substring unsigned 2) 2)]
    [else
     ;; Con punto o exponente es un flotante; si no, un entero exacto.
     (define v (string->number t 10))
     (unless v (kdl-error line col "número inválido: ~a" s))
     (if (regexp-match? #rx"[.eE]" t) (exact->inexact v) v)]))

;; ------------------------------------------------ lectura manual del puerto

(define (space-char? c)
  (and (char? c)
       (or (memv c '(#\tab #\space #\u00A0 #\u1680 #\u202F #\u205F #\u3000))
           (char<=? #\u2000 c #\u200A))))

(define (newline-char? c)
  (and (char? c) (memv c '(#\return #\newline #\vtab #\page #\u0085 #\u2028 #\u2029)) #t))

;; Consume un salto de línea, tratando CRLF como uno solo.
(define (read-newline! port c)
  (when (and (char=? c #\return) (eqv? (peek-char port) #\newline))
    (read-char port)))

;; Consume un comentario `/* */`, que puede anidar. Se entra con `/*` ya leído.
(define (skip-block-comment! port line col)
  (let loop ([depth 1])
    (define c (read-char port))
    (cond
      [(eof-object? c) (kdl-error line col "comentario /* sin cerrar")]
      [(and (char=? c #\*) (eqv? (peek-char port) #\/))
       (read-char port)
       (unless (= depth 1) (loop (sub1 depth)))]
      [(and (char=? c #\/) (eqv? (peek-char port) #\*))
       (read-char port)
       (loop (add1 depth))]
      [else (loop depth)])))

;; Consume `//...` hasta el salto de línea (sin incluirlo: el lexer emite
;; NEWLINE por el comentario mismo, que es su rol en la gramática).
(define (skip-line-comment! port)
  (let loop ()
    (define c (peek-char port))
    (unless (or (eof-object? c) (newline-char? c))
      (read-char port)
      (loop))))

;; ------------------------------------------------------- strings: escapes

(define simple-escapes
  (hasheqv #\" #\" #\\ #\\ #\b #\backspace #\f #\page
           #\n #\newline #\r #\return #\t #\tab #\s #\space))

;; Resuelve los escapes que quedaron crudos tras la primera pasada. Se corre
;; al final porque en un multi-line string el dedent debe ocurrir antes
;; (así `\t` nunca cuenta como sangrado).
(define (resolve-escapes s line col)
  (define out (open-output-string))
  (define n (string-length s))
  (let loop ([i 0])
    (cond
      [(>= i n) (get-output-string out)]
      [(char=? (string-ref s i) #\\)
       (when (>= (add1 i) n) (kdl-error line col "escape incompleto"))
       (define c (string-ref s (add1 i)))
       (cond
         [(hash-ref simple-escapes c #f)
          => (λ (r) (write-char r out) (loop (+ i 2)))]
         [(char=? c #\u)
          (unless (and (< (+ i 2) n) (char=? (string-ref s (+ i 2)) #\{))
            (kdl-error line col "escape \\u debe seguir con {"))
          (define close (let scan ([j (+ i 3)])
                          (cond [(>= j n) (kdl-error line col "escape \\u{ sin cerrar")]
                                [(char=? (string-ref s j) #\}) j]
                                [else (scan (add1 j))])))
          (define hex (substring s (+ i 3) close))
          (define v (string->number hex 16))
          (unless (<= 1 (string-length hex) 6)
            (kdl-error line col "un escape unicode lleva entre 1 y 6 dígitos: \\u{~a}" hex))
          (unless (and v (exact-nonnegative-integer? v)
                       (or (< v #xD800) (< #xDFFF v #x110000)))
            (kdl-error line col "escape unicode inválido: \\u{~a}" hex))
          (write-char (integer->char v) out)
          (loop (add1 close))]
         [else (kdl-error line col "escape desconocido: \\~a" c)])]
      [else (write-char (string-ref s i) out) (loop (add1 i))])))

;; --------------------------------------------------------- strings: dedent

;; Aplica a un multi-line string la regla de sangrado de la spec: la última
;; línea fija el prefijo de whitespace que se le quita a todas las demás.
(define (dedent body line col)
  (define lines (split-kdl-lines body))
  (when (null? lines)
    (kdl-error line col "multi-line string mal formado"))
  ;; El cuerpo debe abrir con un salto de línea y cerrar con una línea que
  ;; sólo tenga whitespace; esas dos líneas no forman parte del valor.
  (unless (string=? (car lines) "")
    (kdl-error line col
               "un multi-line string debe empezar con un salto de línea"))
  (define last-line (last lines))
  (unless (for/and ([c (in-string last-line)]) (space-char? c))
    (kdl-error line col
               "la línea de cierre de un multi-line string sólo puede tener espacios"))
  (define prefix last-line)
  (define inner (drop-right (cdr lines) 1))
  (string-join
   (for/list ([ln (in-list inner)])
     (cond
       ;; Una línea de puro whitespace siempre vale como línea vacía.
       [(for/and ([c (in-string ln)]) (space-char? c)) ""]
       [(string-prefix? ln prefix) (substring ln (string-length prefix))]
       [else (kdl-error line col
                        "sangrado inconsistente en multi-line string: ~s" ln)]))
   "\n"))

;; Corta por cualquiera de los saltos de línea de KDL (CRLF cuenta como uno).
(define (split-kdl-lines s)
  (define out '())
  (define cur (open-output-string))
  (define n (string-length s))
  (let loop ([i 0])
    (cond
      [(>= i n)
       (reverse (cons (get-output-string cur) out))]
      [(newline-char? (string-ref s i))
       (set! out (cons (get-output-string cur) out))
       (set! cur (open-output-string))
       (loop (if (and (char=? (string-ref s i) #\return)
                      (< (add1 i) n)
                      (char=? (string-ref s (add1 i)) #\newline))
                 (+ i 2)
                 (add1 i)))]
      [else (write-char (string-ref s i) cur) (loop (add1 i))])))

;; ------------------------------------------- code points prohibidos

;; La spec veta ciertos code points como caracteres *literales* del documento
;; (escritos con `\u{...}` dentro de un string siguen siendo válidos): los
;; controles C0 y C1 que no son whitespace, DELETE, los controles de dirección
;; bidireccional —que pueden hacer que un documento se lea distinto de como se
;; interpreta— y el BOM salvo al principio del archivo.
(define (disallowed-literal? c)
  (define i (char->integer c))
  (or (<= #x0000 i #x0008)
      (<= #x000E i #x001F)
      (= i #x007F)
      (<= #x200E i #x200F)
      (<= #x202A i #x202E)
      (<= #x2066 i #x2069)
      (= i #xFEFF)))

;; Recorre el documento una vez y ubica el primer code point prohibido.
(define (check-literal-code-points! str)
  (for/fold ([line 1] [col 0] #:result (void))
            ([c (in-string str)])
    (when (disallowed-literal? c)
      (kdl-error line col "code point no permitido en un documento KDL: U+~a"
                 (string-upcase (number->string (char->integer c) 16))))
    (if (newline-char? c)
        (values (add1 line) 0)
        (values line (add1 col)))))

;; ------------------------------------------------- strings: lectura cruda

;; Lee el cuerpo de un string con comillas hasta el delimitador de cierre.
;; Devuelve el texto con los whitespace-escapes ya resueltos y el resto de
;; los escapes todavía crudos, que es lo que necesita el dedent.
(define (read-quoted-body! port multi? line col)
  (define out (open-output-string))
  (let loop ()
    (define c (read-char port))
    (cond
      [(eof-object? c) (kdl-error line col "string sin cerrar")]
      [(char=? c #\")
       (cond
         [(not multi?) (get-output-string out)]
         [(and (eqv? (peek-char port) #\")
               (eqv? (peek-char port 1) #\"))
          (read-char port) (read-char port)
          (get-output-string out)]
         [else (write-char c out) (loop)])]
      [(char=? c #\\)
       (define d (peek-char port))
       (cond
         [(eof-object? d) (kdl-error line col "escape incompleto")]
         ;; ws-escape: la barra y todo el whitespace que sigue desaparecen.
         [(or (space-char? d) (newline-char? d))
          (let skip ()
            (define e (peek-char port))
            (when (and (char? e) (or (space-char? e) (newline-char? e)))
              (read-char port)
              (skip)))
          (loop)]
         [else (write-char c out) (write-char (read-char port) out) (loop)])]
      [(newline-char? c)
       (unless multi?
         (kdl-error line col "un string de una línea no puede contener un salto de línea"))
       (read-newline! port c)
       (write-char #\newline out)
       (loop)]
      [else (write-char c out) (loop)])))

;; Lee el cuerpo de un raw string: sin escapes, cierra con `"` seguido de
;; tantos `#` como abrieron.
(define (read-raw-body! port hashes multi? line col)
  (define closing (make-string hashes #\#))
  (define out (open-output-string))
  (let loop ()
    (define c (read-char port))
    (cond
      [(eof-object? c) (kdl-error line col "raw string sin cerrar")]
      [(char=? c #\")
       (define want (if multi? (string-append "\"\"" closing) closing))
       (cond
         [(equal? (peek-string (string-length want) 0 port) want)
          (read-string (string-length want) port)
          (get-output-string out)]
         [else (write-char c out) (loop)])]
      [(newline-char? c)
       (unless multi?
         (kdl-error line col
                    "un raw string de una línea no puede contener un salto de línea"))
       (read-newline! port c)
       (write-char #\newline out)
       (loop)]
      [else (write-char c out) (loop)])))

;; Decide si un `"` abre un string de una o de varias líneas y arma el valor.
(define (lex-quoted-string! port line col)
  (define multi? (and (eqv? (peek-char port) #\")
                      (eqv? (peek-char port 1) #\")))
  (when multi? (read-char port) (read-char port))
  (define raw (read-quoted-body! port multi? line col))
  (resolve-escapes (if multi? (dedent raw line col) raw) line col))

(define (lex-raw-string! port hashes line col)
  (define multi? (and (eqv? (peek-char port) #\")
                      (eqv? (peek-char port 1) #\")))
  (when multi? (read-char port) (read-char port))
  (define raw (read-raw-body! port hashes multi? line col))
  (if multi? (dedent raw line col) raw))

;; ----------------------------------------------------------------- lexer

;; Las reglas se ordenan de forma que, ante lexemas de igual longitud, gane
;; la interpretación correcta: los números antes que los identificadores
;; (para que `-1` sea un número), y las keywords antes que los raw strings
;; (para que `#true` no se lea como la apertura de un `#"..."#`).
(define kdl-lexer
  (lexer-src-pos
   [(:: #\return #\newline) (token-NEWLINE)]
   [newline-char (token-NEWLINE)]
   [(:+ unicode-space) (token-WS)]

   ;; escline: el salto de línea queda absorbido, el nodo continúa.
   [(:: #\\ (:* unicode-space))
    (let ()
      (define p (peek-string 2 0 input-port))
      (when (equal? p "//") (read-string 2 input-port) (skip-line-comment! input-port))
      (define c (peek-char input-port))
      (cond
        [(eof-object? c) (token-WS)]
        [(newline-char? c) (read-newline! input-port (read-char input-port)) (token-WS)]
        [else (kdl-error (position-line start-pos) (position-col start-pos)
                         "una continuación de línea debe terminar la línea")]))]

   ["/-" (token-SLASHDASH)]
   ["//" (begin (skip-line-comment! input-port) (token-NEWLINE))]
   ["/*" (begin (skip-block-comment! input-port
                                     (position-line start-pos)
                                     (position-col start-pos))
                (token-WS))]

   [#\{ (token-LBRACE)]
   [#\} (token-RBRACE)]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\; (token-SEMI)]
   [#\= (token-EQ)]

   ["#true"  (token-KEYWORD #t)]
   ["#false" (token-KEYWORD #f)]
   ["#null"  (token-KEYWORD 'null)]
   ["#inf"   (token-KEYWORD +inf.0)]
   ["#-inf"  (token-KEYWORD -inf.0)]
   ["#nan"   (token-KEYWORD +nan.0)]

   [(:or hex-num octal-num binary-num decimal-num)
    (token-NUMBER (lexeme->number lexeme
                                  (position-line start-pos)
                                  (position-col start-pos)))]

   [(:+ #\#)
    (let ([line (position-line start-pos)] [col (position-col start-pos)])
      (unless (eqv? (peek-char input-port) #\")
        (kdl-error line col "se esperaba un raw string después de ~a" lexeme))
      (read-char input-port)
      (token-STRING (lex-raw-string! input-port (string-length lexeme) line col)))]

   [#\"
    (token-STRING (lex-quoted-string! input-port
                                      (position-line start-pos)
                                      (position-col start-pos)))]

   [(:+ ident-char)
    (let ([line (position-line start-pos)] [col (position-col start-pos)])
      (unless (valid-identifier? lexeme)
        (if (member lexeme reserved-words)
            (kdl-error line col
                       "`~a` es palabra reservada; en KDL v2 se escribe `#~a`"
                       lexeme lexeme)
            (kdl-error line col "identificador inválido: ~a" lexeme)))
      (token-STRING lexeme))]

   [(eof) (token-EOF)]
   [any-char (kdl-error (position-line start-pos) (position-col start-pos)
                        "carácter inesperado: ~s" lexeme)]))

;; Devuelve la lista completa de position-tokens, terminada en EOF.
(define (lex-kdl in)
  (define text (if (string? in) in (port->string in)))
  ;; Un BOM inicial es legal y no forma parte del documento; en cualquier
  ;; otra posición está prohibido, así que se saca antes de validar.
  (define body (if (and (> (string-length text) 0)
                        (char=? (string-ref text 0) #\uFEFF))
                   (substring text 1)
                   text))
  (check-literal-code-points! body)
  (define port (open-input-string body))
  (port-count-lines! port)
  (let loop ([acc '()])
    (define t (kdl-lexer port))
    (if (eq? (token-name (position-token-token t)) 'EOF)
        (reverse (cons t acc))
        (loop (cons t acc)))))
