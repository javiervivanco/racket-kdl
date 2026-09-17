# kdl

**English** · [Español](README.es.md)

A lexer, parser, AST and plain-data converter for
[KDL v2.0.0](https://kdl.dev/spec/) in pure Racket, using only the standard
libraries (`parser-tools/lex`, `racket/match`, `racket/contract`). No external
dependencies and no shelling out to command-line tools.

It passes all **338 cases** of the
[official KDL conformance suite](https://github.com/kdl-org/kdl/tree/main/tests/test_cases):
each of the 243 valid documents parses to an AST equal to the one produced by
its canonical form, and all 95 invalid documents are rejected.

## Install

```
raco pkg install https://github.com/javiervivanco/racket-kdl.git
```

## Two representations

**AST** — precise, with structs and contracts:

```racket
(require kdl)

(parse-kdl "server port=8080 { route \"/api\" }")
(parse-kdl-node "package name=\"kdl\"")   ; shortcut for a single node
```

**kexpr** — plain data, in the spirit of `jsexpr`, for reading configuration:

```racket
(kdl->kexpr "server port=8080 tls=#true")
```
```racket
(list (hasheq 'name 'server
              'args '()
              'props (hasheq 'port 8080 'tls #t)
              'children '()))
```
```racket
(kexpr->kdl (hasheq 'name 'server 'props (hasheq 'port 8080)))
;; => "server port=8080"
```

A document is a list of nodes; each node is a hash with `'name`, `'args`,
`'props` and `'children`, plus `'type` when it carries an annotation. Names —
of the node, of its properties, of a type annotation — are **symbols**, the way
the keys of a `jsexpr` are; strings are left for data.

A value is a string, a number, a boolean or `'null`. A value that carries a
type annotation is wrapped in `(hasheq 'type ... 'value ...)`, which is the
only place the annotation could go without losing it:

```racket
(kdl->kexpr "created (date-time)\"2024-01-15T10:00:00Z\" size (u8)255")
;; args: (list (hasheq 'type 'date-time 'value "2024-01-15T10:00:00Z")
;;             (hasheq 'type 'u8 'value 255))
```

A bare word and a quoted string are the **same** value in KDL — `n foo`,
`n "foo"` and `n #"foo"#` all mean the same thing — so that distinction does
not survive into a kexpr.

The conversion is lossless: KDL's native types and its type annotations both
survive a round trip.

```racket
(equal? (kdl->kexpr doc) (kdl->kexpr (kexpr->kdl (kdl->kexpr doc))))  ; => #t
```

`kexpr->kdl` quotes strings only when it has to, escapes what cannot appear
literally, and sorts properties by key so the output is reproducible. Names may
be given as strings as well as symbols: reading is strict, writing is lenient.

## AST

Every struct is `#:transparent`, so `equal?` compares by content.

| struct | fields |
|---|---|
| `kdl-document` | `nodes` |
| `kdl-node` | `type` `name` `args` `props` `children` |
| `kdl-argument` | `value` |
| `kdl-property` | `name` `value` |
| `kdl-value` | `type` `datum` |

Contracts are enforced at the module boundary: a malformed tree is rejected
where it is built, including the uniqueness of property keys within a node.

Accessors: `(kdl-node-ref node name [default])` and `(kdl-node-arg-data node)`.

## What is supported

- **Values**: bare identifiers, quoted strings, raw strings with any number of
  `#` delimiters, multi-line strings in both flavours and their indentation
  rule, numbers in decimal, hex, octal and binary with `_` separators and
  exponents, and `#true` `#false` `#null` `#inf` `#-inf` `#nan`.
- **Escapes**: `\n \r \t \\ \" \b \f \s`, `\u{...}` (one to six digits,
  validated to be a Unicode scalar value) and whitespace escapes.
- **Type annotations** on nodes, arguments and properties.
- **Comments**: `//`, `/* */` which nests, and the slashdash `/-` over an
  argument, a property, a block of children or a whole node.
- **Line continuations** with `\`, even followed by a comment.
- **Disallowed literal code points** (controls, DELETE, bidirectional
  direction-control characters, BOM anywhere but the start).
- **Duplicate properties**: the rightmost occurrence wins; positional arguments
  keep their order.

## Errors

Every syntax error raises `exn:fail:kdl`, which carries `line` and `col`:

```racket
> (parse-kdl "node true")
kdl: `true` es palabra reservada; en KDL v2 se escribe `#true` (línea 1, columna 6)
```

## Modules

| module | contents |
|---|---|
| `kdl` | the public API; re-exports all of the others |
| `kdl/ast` | the AST structs and their contracts |
| `kdl/lexer` | the lexer and `exn:fail:kdl` |
| `kdl/parser` | `parse-kdl` / `parse-kdl-node` |
| `kdl/kexpr` | `kdl->kexpr` / `kexpr->kdl` |

## Documentation

Reference documentation in Scribble (`scribblings/kdl.scrbl`):

```
raco docs kdl
```

## Tests

```
raco test tests/
```

- `tests/parser.rkt` — lexer, parser, AST and contracts, including the cases
  derived from the official conformance suite.
- `tests/kexpr.rkt` — the conversion in both directions.
- `tests/docs.rkt` — builds the Scribble document, which evaluates all of its
  examples: if one stops producing the documented result, the test fails.

## License

MIT
