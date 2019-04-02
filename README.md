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
