package org.benknoble.loner.ebnf

import scala.util.parsing.combinator._

object EbnfParser extends RegexParsers {

  protected val commentPrefix = "#"
  override protected val whiteSpace =
    s"""(\\s|${commentPrefix}.*)+""".r

  def epsilon: Parser[Expr] = "ε" ^^ { _ => ε }

  def terminal: Parser[Expr] = """[^<>\[\]{}()ε|;\s]+""".r ^^ { Terminal(_) }

  def nonterminal: Parser[Nonterminal] = """<[^>]+>""".r ^^ { bracketed =>
    val name = bracketed.stripPrefix("<").stripSuffix(">")
    Nonterminal(Symbol(name))
  }

  def opt: Parser[Expr] = "[" ~> exp <~ "]" ^^ { Option(_) }

  def repetition: Parser[Expr] = "{" ~> exp <~ "}" ^^ { Repetition(_) }

  def group: Parser[Expr] = "(" ~> exp <~ ")"

  def sequence: Parser[Expr] =
    (epsilon
      | nonterminal
      | terminal
      | opt
      | repetition
      | group).+ ^^ Expr.sequencify

  def alternation: Parser[Expr] =
    rep1sep(sequence, "|") ^^ Expr.branchify

  def exp: Parser[Expr] =
    alternation.+ ^^ Expr.sequencify

  def goesTo: Parser[Any] = """::="""

  def rule: Parser[Production] =
    nonterminal ~ goesTo ~ exp ~ ";" ^^ {
      case symbol ~ _ ~ rule ~ _ => new Production(symbol, rule)
    }

  def grammar: Parser[Grammar] = rep(rule) ^^ { new Grammar(_) }

  def root: Parser[Grammar] = phrase(grammar)

  def apply(input: String): Either[String, Grammar] = parse(root, input) match {
    case Success(g, _) => Right(g)
    case NoSuccess(msg, _) => Left(msg)
  }

}

// object Main extends App {
//   val grammar = """<A> ::= [a|ε]c ;
// <B> ::= <A>b ;
// <C> ::= {<B>}$ ;
// <D> ::= abd ;
// <E> ::= (a|b)c ;
// <F> ::= a
// | b | c
// | d ;
// """
//   println(EbnfParser(grammar).map(_.format))
// }
