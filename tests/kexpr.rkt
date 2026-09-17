#lang racket/base
;; Pruebas de la conversión KDL <-> kexpr.

(require rackunit racket/math kdl)

(define (one s) (car (kdl->kexpr s)))
(define (roundtrip s) (kdl->kexpr (kexpr->kdl (kdl->kexpr s))))

(test-case "forma de un nodo: las cuatro claves siempre están"
  (check-equal? (one "node")
                (hasheq 'name 'node 'args '() 'props (hasheq) 'children '()))
  (check-equal? (one "node 1 2 k=\"v\" { hijo; }")
                (hasheq 'name 'node
                        'args '(1 2)
                        'props (hasheq 'k "v")
                        'children (list (hasheq 'name 'hijo 'args '()
                                                'props (hasheq) 'children '())))))

  ;; Un valor sin comillas y uno entrecomillado son el mismo string para KDL.
  (check-equal? (one "n foo") (one "n \"foo\""))
  (check-equal? (one "n foo") (one "n #\"foo\"#"))

(test-case "los tipos nativos de KDL sobreviven"
  ;; Esto es lo que la conversión vía XML no podía representar.
  (check-equal? (hash-ref (one "n 42 2.5 #true #false #null") 'args)
                (list 42 2.5 #t #f 'null))
  (check-equal? (hash-ref (one "n 0xff 0o17 0b1010 1_000") 'args)
                (list 255 15 10 1000))
  (check-equal? (hash-ref (one "n #inf #-inf") 'args) (list +inf.0 -inf.0))
  (check-true (nan? (car (hash-ref (one "n #nan") 'args)))))

(test-case "anotaciones de tipo"
  (check-equal? (hash-ref (one "(app)server") 'type) 'app)
  (check-false (hash-ref (one "server") 'type #f))
  (check-equal? (hash-ref (one "n (u8)255") 'args)
                (list (hasheq 'type 'u8 'value 255)))
  (check-equal? (hash-ref (one "n id=(uuid)\"1-2\"") 'props)
                (hasheq 'id (hasheq 'type 'uuid 'value "1-2"))))

(test-case "un documento puede tener varios nodos raíz"
  (check-equal? (map (λ (n) (hash-ref n 'name)) (kdl->kexpr "a; b; c"))
                '(a b c))
  (check-equal? (kdl->kexpr "") '()))

(test-case "ida y vuelta sin pérdida"
  (define doc #<<KDL
(app)servidor "web" puerto=8080 activo=#true nada=#null {
    ruta "/api" metodos="GET,POST"
    limite (u8)255 factor=2.5
    anidado { hoja "x"; otra 1 2 3 }
}
otro-raiz 1
KDL
    )
  (check-equal? (roundtrip doc) (kdl->kexpr doc)))

(test-case "los strings se citan sólo cuando hace falta"
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args '("simple"))) "n simple")
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args '("con espacio")))
                "n \"con espacio\"")
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args '("salto\nlinea")))
                "n \"salto\\nlinea\"")
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args '("comilla\"y\\barra")))
                "n \"comilla\\\"y\\\\barra\"")
  ;; `true` suelto es palabra reservada, así que hay que citarlo.
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args '("true"))) "n \"true\"")
  ;; Un code point prohibido como literal se emite escapado, que sí es legal.
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args '("\u202A")))
                "n \"\\u{202a}\"")
  (check-equal? (hash-ref (one "n \"\\u{202a}\"") 'args) (list "\u202A")))

(test-case "kexpr->kdl tolera nodos incompletos"
  (check-equal? (kexpr->kdl (hasheq 'name 'solo)) "solo")
  (check-equal? (kexpr->kdl (list (hasheq 'name 'a) (hasheq 'name 'b)))
                "a\nb"))

(test-case "la salida es reproducible aunque el hash no tenga orden"
  (define n (hasheq 'name 'n 'props (hasheq 'z 1 'a 2 'm 3)))
  (check-equal? (kexpr->kdl n) "n a=2 m=3 z=1")
  (check-equal? (kexpr->kdl n) (kexpr->kdl n)))

(test-case "la indentación anida"
  (check-equal? (kexpr->kdl (hasheq 'name 'a 'children
                                    (list (hasheq 'name 'b 'children
                                                  (list (hasheq 'name 'c))))))
                "a {\n    b {\n        c\n    }\n}"))

(test-case "errores"
  (check-exn exn:fail:kdl? (λ () (kexpr->kdl (hasheq 'args '(1)))))
  ;; Un nombre puede ser símbolo o string; cualquier otra cosa es error.
  (check-equal? (kexpr->kdl (hasheq 'name "string-tambien")) "string-tambien")
  (check-exn exn:fail:kdl? (λ () (kexpr->kdl (hasheq 'name 42))))
  (check-exn exn:fail:kdl? (λ () (kexpr->kdl (hasheq 'name 'n 'args (list (list 1))))))
  (check-exn exn:fail:kdl? (λ () (kexpr->kdl (hasheq 'name 'n 'args '(1+2i)))))
  ;; Un KDL inválido falla al entrar, con línea y columna.
  (check-exn exn:fail:kdl? (λ () (kdl->kexpr "a { b"))))

(test-case "los racionales exactos se vuelven inexactos, que es lo que KDL tiene"
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args (list 1/2))) "n 0.5")
  (check-equal? (kexpr->kdl (hasheq 'name 'n 'args (list 10))) "n 10"))
