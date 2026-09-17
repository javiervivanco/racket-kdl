# kdl

[English](README.md) · **Español**

Lexer, parser, AST y conversión a datos planos para
[KDL v2.0.0](https://kdl.dev/spec/) en Racket puro, usando sólo bibliotecas
estándar (`parser-tools/lex`, `racket/match`, `racket/contract`). Sin
dependencias externas ni llamadas a herramientas de línea de comandos.

Pasa los **338 casos** de la
[suite oficial de conformidad de KDL](https://github.com/kdl-org/kdl/tree/main/tests/test_cases):
los 243 documentos válidos producen un AST idéntico al de su forma canónica, y
los 95 inválidos son rechazados.

## Instalación

```
raco pkg install https://github.com/javiervivanco/racket-kdl.git
```

## Dos representaciones

**AST** — precisa, con structs y contratos:

```racket
(require kdl)

(parse-kdl "servidor puerto=8080 { ruta \"/api\" }")
(parse-kdl-node "paquete nombre=\"kdl\"")   ; atajo para un solo nodo
```

**kexpr** — datos planos, al estilo `jsexpr`, para leer configuración:

```racket
(kdl->kexpr "servidor puerto=8080 tls=#true")
```
```racket
(list (hasheq 'name 'servidor
              'args '()
              'props (hasheq 'puerto 8080 'tls #t)
              'children '()))
```
```racket
(kexpr->kdl (hasheq 'name 'servidor 'props (hasheq 'puerto 8080)))
;; => "servidor puerto=8080"
```

Un documento es una lista de nodos; cada nodo es un hash con `'name`, `'args`,
`'props` y `'children`, más `'type` si lleva anotación. Los nombres —del nodo,
de las propiedades y de las anotaciones— son **símbolos**, como las claves de
un `jsexpr`; los strings quedan para los datos.

Un valor es un string, un número, un booleano o `'null`. Si lleva anotación de
tipo se envuelve en `(hasheq 'type ... 'value ...)`, que es el único lugar
donde la anotación puede ir sin perderse:

```racket
(kdl->kexpr "creado (date-time)\"2024-01-15T10:00:00Z\" tamaño (u8)255")
;; args: (list (hasheq 'type 'date-time 'value "2024-01-15T10:00:00Z")
;;             (hasheq 'type 'u8 'value 255))
```

Una palabra sin comillas y un string entrecomillado son el **mismo** valor para
KDL —`n foo`, `n "foo"` y `n #"foo"#` significan lo mismo—, así que esa
distinción no sobrevive al kexpr.

La conversión no pierde nada: los tipos nativos de KDL y las anotaciones de
tipo sobreviven la ida y la vuelta.

```racket
(equal? (kdl->kexpr doc) (kdl->kexpr (kexpr->kdl (kdl->kexpr doc))))  ; => #t
```

`kexpr->kdl` cita los strings sólo cuando hace falta, escapa lo que no puede ir
literal, y ordena las propiedades por clave para que la salida sea
reproducible. Los nombres se aceptan como símbolo o como string: estricto al
leer, tolerante al escribir.

## AST

Todos los structs son `#:transparent`, así que `equal?` compara por contenido.

| struct | campos |
|---|---|
| `kdl-document` | `nodes` |
| `kdl-node` | `type` `name` `args` `props` `children` |
| `kdl-argument` | `value` |
| `kdl-property` | `name` `value` |
| `kdl-value` | `type` `datum` |

Los contratos se aplican en la frontera del módulo: un árbol mal formado se
rechaza al construirlo, incluida la unicidad de las claves de propiedad dentro
de un nodo.

Accesores: `(kdl-node-ref node nombre [default])` y `(kdl-node-arg-data node)`.

## Cobertura de la especificación

- **Valores**: identificadores sin comillas, strings con comillas, raw strings
  con `#` arbitrarios, multi-line strings (con y sin comillas) y su regla de
  sangrado, números decimales/hex/octal/binario con `_` y exponente, y
  `#true` `#false` `#null` `#inf` `#-inf` `#nan`.
- **Escapes**: `\n \r \t \\ \" \b \f \s`, `\u{...}` (de 1 a 6 dígitos,
  validando que sea un Unicode Scalar Value) y escapes de whitespace.
- **Anotaciones de tipo** sobre nodos, argumentos y propiedades.
- **Comentarios**: `//`, `/* */` anidados y el slashdash `/-` sobre un
  argumento, una propiedad, un bloque de hijos o un nodo completo.
- **Continuación de línea** con `\`, incluso seguida de un comentario.
- **Code points prohibidos** como literales (controles, DELETE, controles de
  dirección bidireccional, BOM fuera del inicio).
- **Propiedades duplicadas**: gana la de más a la derecha; los argumentos
  posicionales preservan su orden.

## Errores

Todo error de sintaxis levanta `exn:fail:kdl`, que lleva `line` y `col`:

```racket
> (parse-kdl "nodo true")
kdl: `true` es palabra reservada; en KDL v2 se escribe `#true` (línea 1, columna 6)
```

## Módulos

| módulo | contenido |
|---|---|
| `kdl` | la API pública; reexporta todos los demás |
| `kdl/ast` | structs del AST y sus contratos |
| `kdl/lexer` | lexer y `exn:fail:kdl` |
| `kdl/parser` | `parse-kdl` / `parse-kdl-node` |
| `kdl/kexpr` | `kdl->kexpr` / `kexpr->kdl` |

## Documentación

La documentación de referencia está en Scribble, en inglés
(`scribblings/kdl.scrbl`):

```
raco docs kdl
```

## Tests

```
raco test tests/
```

- `tests/parser.rkt` — lexer, parser, AST y contratos, incluidos los casos
  derivados de la suite oficial de conformidad.
- `tests/kexpr.rkt` — la conversión en ambos sentidos.
- `tests/docs.rkt` — construye el documento Scribble, lo que evalúa todos sus
  ejemplos: si uno deja de dar el resultado documentado, el test falla.

## Licencia

MIT
