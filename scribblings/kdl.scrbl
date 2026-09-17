#lang scribble/manual
@require[@for-label[kdl racket/base racket/contract]
         scribble/example]

@(define ev (make-base-eval))
@(ev '(require kdl))

@title{KDL: a parser for the KDL Document Language}
@author{jvivanco}

@defmodule[kdl]

A lexer, parser and abstract syntax tree for
@hyperlink["https://kdl.dev/spec/"]{KDL v2.0.0}, written in plain Racket using
only the standard libraries: @racketmodname[parser-tools/lex] for the lexical
structure, @racketmodname[racket/match] for the recursive-descent parser, and
@racketmodname[racket/contract] to keep malformed trees from being built in the
first place. Nothing is shelled out to an external tool.

Two levels of representation are available: a precise AST built from structs
(@secref{The_abstract_syntax_tree}), and @tech{kexpr} (@secref{kexpr}), plain
hashes and lists in the spirit of @racket[jsexpr], for when a document is just
configuration to be read.

The implementation passes all 338 cases of the
@hyperlink["https://github.com/kdl-org/kdl/tree/main/tests/test_cases"]{official
KDL conformance suite}: each of the 243 valid documents parses to an AST equal
to the one produced by its canonical form, and all 95 invalid documents are
rejected.

@section{Parsing}

@defproc[(parse-kdl [in (or/c string? input-port?)]) kdl-document?]{
  Parses a complete KDL document and returns its AST. Raises
  @racket[exn:fail:kdl] if the input is not well-formed KDL.

  @examples[#:eval ev
    (parse-kdl "server port=8080")
    (parse-kdl "a; b; c")
  ]
}

@defproc[(parse-kdl-node [in (or/c string? input-port?)]) kdl-node?]{
  Like @racket[parse-kdl], but for a document that holds exactly one node,
  which it returns directly. Raises @racket[exn:fail] if the document has any
  other number of nodes. Convenient in tests and in code that already knows
  the shape of its input.

  @examples[#:eval ev
    (parse-kdl-node "package name=\"kdl\" version=\"0.2\"")
  ]
}

@section{The abstract syntax tree}

Every struct is @racket[#:transparent], so @racket[equal?] compares trees by
content and a test can spell out the expected tree literally.

@defstruct*[kdl-document ([nodes (listof kdl-node?)])]{
  A whole document: the sequence of its top-level nodes.
}

@defstruct*[kdl-node ([type (or/c #f string?)]
                      [name string?]
                      [args (listof kdl-argument?)]
                      [props (listof kdl-property?)]
                      [children (listof kdl-node?)])]{
  A single node. @racket[type] is its type annotation, or @racket[#f] when it
  has none. @racket[args] keeps the positional arguments in source order;
  @racket[props] holds the properties, whose names are guaranteed to be unique
  (see @secref{duplicates}).

  @examples[#:eval ev
    (define n (parse-kdl-node "(app)server \"web\" port=8080 { route \"/api\" }"))
    (kdl-node-type n)
    (kdl-node-name n)
    (kdl-node-arg-data n)
    (map kdl-node-name (kdl-node-children n))
  ]
}

@defstruct*[kdl-argument ([value kdl-value?])]{
  A positional argument.
}

@defstruct*[kdl-property ([name string?] [value kdl-value?])]{
  A @tt{key=value} property.
}

@defstruct*[kdl-value ([type (or/c #f string?)]
                       [datum (or/c string? number? boolean? 'null)])]{
  A value, together with its type annotation if it carries one.

  KDL's @tt{#null} becomes the symbol @racket['null] rather than @racket[#f],
  so that it stays distinguishable from @tt{#false}. @tt{#inf}, @tt{#-inf} and
  @tt{#nan} become the corresponding Racket flonums.

  @examples[#:eval ev
    (kdl-node-args (parse-kdl-node "n (u8)255 #null #inf"))
  ]
}

The contracts are enforced at the module boundary, so a tree that violates them
is rejected where it is built rather than somewhere downstream:

@examples[#:eval ev
  (eval:error (kdl-value #f (list 1 2)))
  (eval:error (kdl-node #f 'not-a-string '() '() '()))
]

@subsection{Accessors}

@defproc[(kdl-node-ref [node kdl-node?] [name string?] [default any/c #f])
         any/c]{
  Looks a property up by name and returns its @racket[kdl-value], or
  @racket[default] if the node has no such property.

  @examples[#:eval ev
    (define n (parse-kdl-node "server port=8080 tls=#true"))
    (kdl-node-ref n "port")
    (kdl-value-datum (kdl-node-ref n "tls"))
    (kdl-node-ref n "missing" 'none)
  ]
}

@defproc[(kdl-node-arg-data [node kdl-node?])
         (listof (or/c string? number? boolean? 'null))]{
  The data carried by the node's positional arguments, in order, with the type
  annotations dropped.

  @examples[#:eval ev
    (kdl-node-arg-data (parse-kdl-node "sizes 1 2.5 \"three\" #true"))
  ]
}

@section[#:tag "kexpr"]{Plain data: kexpr}

The AST is precise but verbose. When a document is just configuration to be
read, @deftech{kexpr} is the flatter alternative: plain Racket data, the way a
@racket[jsexpr] is to JSON. There are no structs and no contracts to satisfy,
only hashes and lists to walk with @racket[hash-ref] and @racket[for].

A document is a list of nodes, and each node is a hash:

@racketblock[
knode  = (hasheq 'name     symbol?
                 'type     symbol?      (code:comment "only when annotated")
                 'args     (listof kvalue)
                 'props    (hasheq symbol? kvalue)
                 'children (listof knode))

kvalue = string? #,(elem "|") number? #,(elem "|") boolean? #,(elem "|") 'null
       #,(elem "|") (hasheq 'type symbol? 'value kvalue)
]

Names — of the node, of its properties, of a type annotation — are symbols,
the way the keys of a @racket[jsexpr] are; strings are left for data.

Note that a bare word and a quoted string are the @emph{same} value in KDL:
@tt{node foo}, @tt{node "foo"} and @tt{node #"foo"#} all mean the same thing,
and the canonical form of a document drops the quotes where they are not
needed. So that distinction does not survive into a kexpr, and should not.

@examples[#:eval ev
  (equal? (kdl->kexpr "n foo") (kdl->kexpr "n \"foo\""))
]

The conversion loses nothing: KDL's native types and its type annotations both
survive a round trip.

@defproc[(kdl->kexpr [in (or/c string? input-port?)]) (listof hash?)]{
  Parses a document and returns it as a list of node hashes. Raises
  @racket[exn:fail:kdl] on malformed input.

  Every node carries all four of @racket['name], @racket['args],
  @racket['props] and @racket['children], so they can be read without
  supplying a default; @racket['type] appears only on annotated nodes.

  A value that carries a type annotation is wrapped in a hash, which is the
  only place the annotation could go without losing it; a plain value is the
  datum itself.

  @examples[#:eval ev
    (kdl->kexpr "server port=8080 tls=#true")
    (kdl->kexpr "a; b")
    (kdl->kexpr "n (u8)255")
  ]
}

@defproc[(kexpr->kdl [k (or/c hash? (listof hash?))]) string?]{
  Renders a kexpr back to KDL text. A single node hash is accepted in place of
  a one-node document, and a node may leave out the keys it does not need.
  Names may be given as strings as well as symbols: reading is strict, writing
  is lenient.

  Strings are quoted only when they have to be — when they are not a legal bare
  identifier — and escapes are inserted where a literal character would be
  illegal. Properties are sorted by key, since a hash has no order of its own
  and the output should be reproducible.

  @examples[#:eval ev
    (display (kexpr->kdl (hasheq 'name 'server
                                 'props (hasheq 'port 8080))))
    (display (kexpr->kdl (hasheq 'name 'a
                                 'children (list (hasheq 'name 'b)))))
    (display (kexpr->kdl (hasheq 'name 'n 'args (list "needs quoting"))))
  ]

  Raises @racket[exn:fail:kdl] if a node has no @racket['name], or if a value
  has no KDL representation.

  @examples[#:eval ev
    (eval:error (kexpr->kdl (hasheq 'args '(1))))
    (eval:error (kexpr->kdl (hasheq 'name 'n 'args (list (list 1 2)))))
  ]
}

The two are inverses, up to formatting:

@examples[#:eval ev
  (define doc "pkg name=\"kdl\" version=2 ok=#true { dep (u8)255 }")
  (display (kexpr->kdl (kdl->kexpr doc)))
  (equal? (kdl->kexpr doc) (kdl->kexpr (kexpr->kdl (kdl->kexpr doc))))
]

@section{What is supported}

@subsection{Values}

Bare identifiers, quoted strings, raw strings with any number of @tt{#}
delimiters, multi-line strings in both flavours, numbers in decimal, hex, octal
and binary (with @tt{_} separators and exponents), and the keywords
@tt{#true}, @tt{#false}, @tt{#null}, @tt{#inf}, @tt{#-inf} and @tt{#nan}.

@examples[#:eval ev
  (kdl-node-arg-data (parse-kdl-node "n 0xff 0o17 0b1010 1_000_000 -2.5e3"))
  (kdl-node-arg-data (parse-kdl-node "n #\"a raw \\n string\"#"))
]

Note that in KDL v2 the bare words @tt{true}, @tt{false}, @tt{null},
@tt{inf}, @tt{-inf} and @tt{nan} are reserved: they must be written with the
leading @tt{#}, and on their own they are an error rather than a string.

@examples[#:eval ev
  (eval:error (parse-kdl "enabled true"))
]

@subsection{Multi-line strings}

The closing line of a multi-line string sets the whitespace prefix that is
stripped from every other line; the opening and closing newlines are not part
of the value, and a line that holds only whitespace always counts as empty.
Inconsistent indentation is an error.

@examples[#:eval ev
  (kdl-node-arg-data
   (parse-kdl-node "text \"\"\"\n    first\n      indented\n\n    last\n    \"\"\""))
]

@subsection{Escapes}

@tt{\n}, @tt{\r}, @tt{\t}, @tt{\\}, @tt{\"}, @tt{\b}, @tt{\f} and @tt{\s}
(a space), plus @tt{\u@"{"...@"}"} with one to six hexadecimal digits, validated
to be a Unicode scalar value. A backslash followed by whitespace discards both
the backslash and the whitespace.

@examples[#:eval ev
  (kdl-node-arg-data (parse-kdl-node "n \"a\\tb\""))
]

@subsection{Comments and slashdash}

@tt{//} to the end of the line, @tt{/* */} which nests, and the slashdash
@tt{/-}, which is a @emph{semantic} comment: whatever follows it is parsed like
anything else — so it still has to be valid — and only then discarded. It
applies to an argument, a property, a block of children, or a whole node.

@examples[#:eval ev
  (parse-kdl-node "flags /-dropped \"kept\" /-key=\"x\" real=\"y\"")
  (map kdl-node-name (kdl-document-nodes (parse-kdl "a\n/-b 1 { deep; }\nc")))
  (eval:error (parse-kdl "/-node { unclosed"))
]

@subsection{Line continuations}

A @tt{\} at the end of a line lets a node carry on to the next one.

@examples[#:eval ev
  (parse-kdl-node "limits \\\n  memory=512 \\\n  cpu=2")
]

@subsection[#:tag "duplicates"]{Duplicate properties}

Properties are processed left to right and the rightmost occurrence of a key
wins, keeping its position. Positional arguments preserve their order strictly.

@examples[#:eval ev
  (kdl-node-props (parse-kdl-node "n x=1 y=2 x=3"))
]

@subsection{Disallowed literal code points}

The specification forbids a set of code points from appearing literally in a
document: the C0 and C1 controls that are not whitespace, DELETE, the
bidirectional direction-control characters, and the byte order mark anywhere
but at the very start. Writing them as an escape is still fine.

@examples[#:eval ev
  (eval:error (parse-kdl "node \"\u202A\""))
  (kdl-node-arg-data (parse-kdl-node "node \"\\u{202A}\""))
]

@section{Errors}

@defstruct*[(exn:fail:kdl exn:fail) ([line (or/c #f exact-positive-integer?)]
                                     [col (or/c #f exact-nonnegative-integer?)])]{
  Raised for every lexical or syntactic error. Besides the message, it carries
  the line and column where the problem was found.

  @examples[#:eval ev
    (eval:error (parse-kdl "ok\nnode \"unterminated"))
    (with-handlers ([exn:fail:kdl? exn:fail:kdl-line])
      (parse-kdl "ok\nnode \"unterminated"))
  ]
}

@section{Modules}

@defmodule[kdl/ast #:no-declare]{The AST structs and their contracts.}
@defmodule[kdl/lexer #:no-declare]{The lexer and @racket[exn:fail:kdl].}
@defmodule[kdl/parser #:no-declare]{@racket[parse-kdl] and @racket[parse-kdl-node].}
@defmodule[kdl/kexpr #:no-declare]{@racket[kdl->kexpr] and @racket[kexpr->kdl].}

@racketmodname[kdl] re-exports all of them and is the module to require.

@section{Tests}

@commandline{raco test tests/}
