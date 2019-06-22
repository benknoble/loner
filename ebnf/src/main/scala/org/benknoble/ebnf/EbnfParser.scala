package org.benknoble.ebnf

import scala.scalajs.js.annotation.{ JSExportTopLevel, JSExport }

import scala.util.parsing.combinator._

/** Parser for EBNF specifications
  *
  * Intended usage is via the `apply` method to Strings, or via standard
  * parser combinator mechanisms. `apply` returns either the error message or
  * the [[org.benknoble.ebnf.Grammar]] in an Either.
  *
  * From [[http://matt.might.net/articles/grammars-bnf-ebnf/ Matt Might's specification]]
  *
  * =BNF=
  *
  * When describing languages, Backus-Naur form (BNF) is a formal notation for
  * encoding grammars intended for human consumption.
  *
  * Many programming languages, protocols or formats have a BNF description in
  * their specification.
  *
  * Every rule in Backus-Naur form has the following structure:
  *
  * {{{
  * name ::= expansion
  * }}}
  *
  * The symbol `::=` means "may expand into" and "may be replaced with."
  *
  * In some texts, a name is also called a non-terminal symbol.
  *
  * Every name in Backus-Naur form is surrounded by angle brackets, `<` `>`,
  * whether it appears on the left- or right-hand side of the rule.
  *
  * An expansion is an expression containing terminal symbols and non-terminal
  * symbols, joined together by sequencing and choice.
  *
  * A terminal symbol is a literal like ("+" or "function") or a class of
  * literals (like integer). It must be quoted using single quotes. Any
  * metacharacters may appear within the quotes.
  *
  * Simply juxtaposing expressions indicates sequencing.
  *
  * A vertical bar `|` indicates choice.
  *
  * For example, in BNF, the classic expression grammar is:
  * {{{
  * <expr> ::= <term> "+" <expr> |  <term>
  * <term> ::= <factor> "*" <term> |  <factor>
  * <factor> ::= "(" <expr> ")" |  <const>
  * <const> ::= integer
  * }}}
  *
  * =EBNF=
  *
  * Extended Backus-Naur form (EBNF) is a collection of extensions to
  * Backus-Naur form.
  *
  * More important than the minor syntactic differences between the forms of
  * EBNF are the additional operations it allows in expansions.
  *
  * ==Option==
  *
  * In EBNF, square brackets around an expansion, `[ expansion ]`, indicates
  * that this expansion is optional.
  *
  * For example, the rule:
  * {{{
  * <term> ::= [ "-" ] <factor>
  * }}}
  * allows factors to be negated.
  *
  * ==Repetition==
  *
  * In EBNF, curly braces indicate that the expression may be repeated zero or
  * more times.
  *
  * For example, the rule:
  * {{{
  * <args> ::= <arg> { "," <arg> }
  * }}}
  * defines a conventional comma-separated argument list.
  *
  * ==Grouping==
  *
  * To indicate precedence, EBNF grammars may use parentheses, `()`, to
  * explictly define the order of expansion.
  *
  * For example, the rule:
  * {{{
  * <expr> ::= <term> ("+" | "-") <expr>
  * }}}
  * defines an expression form that allows both addition and subtraction.
  *
  * =Extensions=
  *
  * This version of EBNF admits a "comment-like" extension syntax: a `#` escapes
  * the rest of the line, such that it is not parsed at all. Indeed, it is not
  * kept in the grammar object at all, so the result of formatting loses the
  * comments (considered a feature: it cleans the output for consumption by
  * another tool).
  *
  * The following grammar from the tests is thus valid:
  * {{{
  * # this is a comment
  * <A> ::= 'a'     # a values
  *         | 'b'   # b values
  *         | 'c'   # c values
  *         ;
  * }}}
  */
@JSExportTopLevel("EbnfParser")
object EbnfParser extends RegexParsers {

  protected val commentPrefix = "#"
  override protected val whiteSpace =
    s"""(\\s|${commentPrefix}.*)+""".r

  /** Parser for ε */
  def epsilon: Parser[Expr] = "ε" ^^ { _ => ε }

  /** Parser for terminals */
  def terminal: Parser[Expr] = """'[^']+'""".r ^^ { quoted =>
    val term = quoted.substring(1,quoted.length-1)
    Terminal(term)
  }

  /** Parser for nonterminals */
  def nonterminal: Parser[Nonterminal] = """<[^>]+>""".r ^^ { bracketed =>
    val name = bracketed.stripPrefix("<").stripSuffix(">")
    Nonterminal(Symbol(name))
  }

  /** Parser for options */
  def opt: Parser[Expr] = "[" ~> exp <~ "]" ^^ { Option(_) }

  /** Parser for repetition */
  def repetition: Parser[Expr] = "{" ~> exp <~ "}" ^^ { Repetition(_) }

  /** Parser for parenthesized groups */
  def group: Parser[Expr] = "(" ~> exp <~ ")"

  /** Parser for a sequence of expressions
    *
    * Anything but an alternation
    */
  def sequence: Parser[Expr] =
    (epsilon
      | nonterminal
      | terminal
      | opt
      | repetition
      | group).+ ^^ Expr.sequencify

  /** Parser for an alternation
    *
    * Top level of stratified gramamr: basically repeated sequences separated by
    * pipes
    */
  def alternation: Parser[Expr] =
    rep1sep(sequence, "|") ^^ Expr.branchify

  /** Parser for top-level expression */
  def exp: Parser[Expr] =
    alternation.+ ^^ Expr.sequencify

  /** Parser for `::=` */
  def goesTo: Parser[Any] = """::="""

  /** Parser for productions */
  def rule: Parser[Production] =
    nonterminal ~ goesTo ~ exp ~ ";" ^^ {
      case symbol ~ _ ~ rule ~ _ => new Production(symbol, rule)
    }

  /** Parser for grammar */
  def grammar: Parser[Grammar] = rep(rule) ^^ { new Grammar(_) }

  /** Parser for top-level Gramamr
    *
    * Considers the entire input as a grammar, failing otherwise
    *
    * @see [[org.benknoble.ebnf.EbnfParser.phrase]]
    */
  def root: Parser[Grammar] = phrase(grammar)

  /** Parses a grammar from a String
    *
    * A fun trick is
    * {{{
    * println(EbnfParser(grammar).map(_.format))
    * }}}
    *
    * @return Either the error message or the grammar
    */
  def apply(input: String): Either[String, Grammar] = parse(root, input) match {
    case Success(g, _) => Right(g)
    case NoSuccess(msg, _) => Left(msg)
  }

  @JSExport
  protected def parseJS_scala(input: String): String =
    apply_fold(input)(_.toString())

  @JSExport
  protected def parseJS_format(input: String): String =
    apply_fold(input)(_.format)

  @JSExport
  protected def parseJS_grammar(input: String): Grammar =
    apply(input).fold(
      _ => Grammar(),
      grammar => grammar)

  private def apply_fold(input: String)(f: Grammar => String) =
    apply(input).fold(
      str => str,
      grammar => f(grammar))

}

// object Main extends App {
//   val grammar = """<A> ::= ['a'|ε]'c' ;
// <B> ::= <A>'b' ;
// <C> ::= {<B>}'$' ;
// <D> ::= 'abd' ;
// <E> ::= ('a'|'b')'c' ;
// <F> ::= 'a'
// | 'b' | 'c'
// | 'd' ;
// """
//   println(EbnfParser(grammar).map(_.format))
// }
