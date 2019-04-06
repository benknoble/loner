# Loner

An EBNF parser and LL(1) checker, written in Scala.

This project consists of

  - [`ebnf`](./ebnf/): an EBNF parser and formatting filter
  - **COMING SOON** [`loner`](./src/): an LL(1) checker that depends on the
    `ebnf` library

Each project is split into an API component and a `Main.scala` CLI component.
The CLI driver serves as an example of the API usage, as a proof-of-concept,
and as a useful tool for developers (especially those working on language and
compiler design).

## Getting Started

See the self-documenting [examples](./examples)!

## EBNF File Format

See the docs for the Parser. In brief:

From [Matt Might's specification](http://matt.might.net/articles/grammars-bnf-ebnf/)

Rules look like `name ::= expansion`

Every name is surrounded by angle brackets, `<` `>`.

An expansion is an expression containing terminal symbols and non-terminal
symbols, joined together by sequencing and choice.

A terminal symbol is a literal like ("+" or "function") or a class of
literals (like integer).

Simply juxtaposing expressions indicates sequencing.

A vertical bar `|` indicates choice.

Example:
```
<expr> ::= <term> "+" <expr> |  <term>
<term> ::= <factor> "*" <term> |  <factor>
<factor> ::= "(" <expr> ")" |  <const>
<const> ::= integer
```

Square brackets around an expansion, `[ expansion ]`, indicates that this
expansion is optional.

For example, the rule:
```
<term> ::= [ "-" ] <factor>
```
allows factors to be negated.

Curly braces indicate that the expression may be repeated zero or more times.

For example, the rule:
```
<args> ::= <arg> { "," <arg> }
```
defines a conventional comma-separated argument list.

To indicate precedence, use parentheses, `()`, to explictly define the order of
expansion.

For example, the rule:
```
<expr> ::= <term> ("+" | "-") <expr>
```
defines an expression form that allows both addition and subtraction.

This version of EBNF admits a "comment-like" extension syntax: a `#` escapes
the rest of the line, such that it is not parsed at all. Indeed, it is not
kept in the grammar object at all, so the result of formatting loses the
comments (considered a feature: it cleans the output for consumption by
another tool).

The following grammar from the tests is thus valid:
```
# this is a comment
<A> ::= a     # a values
        | b   # b values
        | c   # c values
        ;
```

All whitespace is ignored.

## Documentation

Generated HTML documentation can be found on the [website][site].

**COMING SOON** Check out the live demo!

## Developer Guide

See primarily [build.sbt](./build.sbt), as well as

  - [Dependencies](./project/Dependencies.scala)
  - [assembly](./project/assembly.sbt)

Loner is configured and built using `sbt`, and conforms to all the standard
commands. Because it provides main methods, the project code may be run via
`run`.

The two primary projects are `root` (the loner project) and `ebnf`. Switch
between them with `project`; root depends on ebnf, for ease of development.

Documentation is generated via `doc`. `sbt-site` and `sbt-ghpages` provide
site-generation tools for the [website][site].

Tests are written using `scalatest` and are accessible via the standard `test`
commands. All API features require tests—new features or bug-fixes should ensure
test coverage, in the style of the originals.

It also uses the `assembly` plugin (providing an `assembly` command), which is
used to generated FAT executable jars—these jars have no dependencies other than
a working java runtime environment, and are uploaded in the releases section.
They can be executed directly (`chmod -u+x <jar> ; ./<jar>`) or via java (`java
-jar <jar>`).

### Credits

This project was built while enrolled in Comp 520 at UNC Chapel Hill, taught by
Dr. Jan Prins, in Spring 2019. The topics of this compilers course inspired
loner.

[site]: https://benknoble.github.io/loner/
