# Loner

[![This project is considered stable](https://img.shields.io/badge/status-stable-success.svg)](https://benknoble.github.io/status/stable/)

An EBNF parser and LL(1) checker, written in Scala.

This project consists of

  - [`ebnf`](./ebnf/src/): an EBNF parser and formatting filter
  - [`loner`](./src/): an LL(1) checker that depends on the
    `ebnf` library

Each project is split into an API component and a `Main.scala` CLI component.
The CLI driver serves as an example of the API usage, as a proof-of-concept,
and as a useful tool for developers (especially those working on language and
compiler design).

The CLI drivers have the same interface: they read from standard in, or from a
file if passed one as a parameter. They output to standard out a formatted
grammar, suitable for machine consumption, or an error message if the input is
not EBNF-formatted (see below). Loner additionally errors if the grammar is not
LL(1). The exit status reflects the success state (0: success, 1: not ebnf, 2:
invalid arguments). Given the `-h` flags gives a help message. Given the `-q`
suppresses output.

## Getting Started

You can find pre-built executables in the Releases section.

Run the tools on some [examples](./examples)!

## Documentation

Generated HTML documentation can be found on the [website][site].

Check out the [live demo][demo]!

## EBNF File Format

See the docs for the EbnfParser. In brief:

From [Matt Might's specification](http://matt.might.net/articles/grammars-bnf-ebnf/)

 - Rules look like `name ::= expansion`

 - Every name is surrounded by angle brackets, `<` `>`.

 - An expansion is an expression containing terminal symbols and non-terminal
   symbols, joined together by sequencing and choice.

 - A terminal symbol is a literal like ("+" or "function") or a class of
   literals (like integer). It must be in single-quotes.

 - Simply juxtaposing expressions indicates sequencing.

 - A vertical bar `|` indicates choice.

Example:
```
<expr> ::= <term> '+' <expr> |  <term>
<term> ::= <factor> '*' <term> |  <factor>
<factor> ::= '(' <expr> ')' |  <const>
<const> ::= 'integer'
```

 - Square brackets around an expansion, `[ expansion ]`, indicates that this
   expansion is optional.

For example, the rule:
```
<term> ::= [ '-' ] <factor>
```
allows factors to be negated.

 - Curly braces indicate that the expression may be repeated zero or more times.

For example, the rule:
```
<args> ::= <arg> { ',' <arg> }
```
defines a conventional comma-separated argument list.

 - To indicate precedence, use parentheses, `()`, to explictly define the order
   of expansion.

For example, the rule:
```
<expr> ::= <term> ('+' | '-') <expr>
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
<A> ::= 'a'     # a values
        | 'b'   # b values
        | 'c'   # c values
        ;
```

All whitespace is ignored.

## Developer Guide

See primarily [build.sbt](./build.sbt), as well as

  - [Dependencies](./project/Dependencies.scala)
  - [plugins](./project/plugins.sbt)

Loner is configured and built using `sbt`, and conforms to all the standard
commands. Because it provides main methods, the project code may be run via
`run`.

The two primary projects are `loner` and `ebnf`. Switch between them with
`project`; loner depends on ebnf, for ease of development.

Documentation is generated via `scaladocSite`. `sbt-site` and `sbt-ghpages` provide
site-generation tools for the [website][site].

Tests are written using `scalatest` and are accessible via the standard `test`
commands. All API features require tests—new features or bug-fixes should ensure
test coverage, in the style of the originals.

It also uses the `assembly` plugin (providing an `assembly` command), which is
used to generated FAT executable jars—these jars have no dependencies other than
a working java runtime environment, and are uploaded in the releases section.
They can be executed directly (`chmod -u+x <jar> ; ./<jar>`) or via java (`java
-jar <jar>`).

### Bugs

Does not support escaping special characters (yet).

### Credits

This project was built while enrolled in Comp 520 at UNC Chapel Hill, taught by
Dr. Jan Prins, in Spring 2019. The topics of this compilers course inspired
loner.

[site]: https://benknoble.github.io/loner/
[demo]: https://benknoble.github.io/loner/demo.html
