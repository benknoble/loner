package org.benknoble.loner.ebnf

import scala.util.parsing.combinator._

class EbnfParser extends RegexParsers {

  def epsilon: Parser[Expr] = "ε" ^^ { _ => ε }

  def terminal: Parser[Expr] = """[^<>\[\]{}()ε|;\s]+""".r ^^ { quoted =>
    val str = quoted.stripPrefix("\"").stripSuffix("\"")
    Terminal(str)
  }

  def nonterminal: Parser[Nonterminal] = """<[^>]+>""".r ^^ { bracketed =>
    val name = bracketed.stripPrefix("<").stripSuffix(">")
    Nonterminal(Symbol(name))
  }

  def opt: Parser[Expr] = "[" ~> exp <~ "]" ^^ { Option(_) }

  def repetition: Parser[Expr] = "{" ~> exp <~ "}" ^^ { Repetition(_) }

  def group: Parser[Expr] = "(" ~> exp <~ ")"

  def alternation: Parser[Expr] =
    chainl1(epsilon
      | nonterminal
      | terminal
      | opt
      | repetition
      | group,
      "|" ^^^ { (left: Expr, right: Expr) => Alternation(left, right) } )

  def sequence: Parser[Expr] =
    alternation.+ ^^ Expr.sequencify

  def exp: Parser[Expr] =
    sequence.+ ^^ Expr.sequencify

  def goesTo: Parser[Any] = """::="""

  def rule: Parser[Production] =
    nonterminal ~ goesTo ~ exp ~ ";" ^^ {
      case symbol ~ _ ~ rule ~ _ => new Production(symbol, rule)
    }

  def grammar: Parser[Grammar] = rep(rule) ^^ { new Grammar(_) }

  def root: Parser[Grammar] = phrase(grammar)

  // Should eventually be a custom error type ?
  // Or do I just return the results? # = parse(root, grammar)
  def parse(grammar: String): Either[String, Grammar] = parse(root, grammar) match {
    case Success(result, _) => Right(result)
    case Failure(msg, _) => Left(msg)
    case Error(msg, _) => Left(msg)
  }

}

object Main extends App {
  val grammar = """<A> ::= [a|ε]c ;
<B> ::= <A>b ;
<C> ::= {<B>}$ ;
<D> ::= abd ;
<E> ::= (a|b)c ;
<F> ::= a
| b | c
| d ;
"""
  // val parser = new EbnfParser()
  // val rule = parser.root
  // println(parser.parse(rule, grammar))
  println(new EbnfParser().parse(grammar).fold(s => s, g => g.format))
}
