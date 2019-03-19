package org.benknoble.loner.ebnf

import scala.language.implicitConversions

import ExprImplicts._

abstract class Expr {
  def ~ (right: Expr): Expr = Sequence(this, right)
  def || (right: Expr): Expr = Alternation(this, right)
  def *(): Expr = Repetition(this)
  def ?(): Expr = Option(this)

  def format: String
}

abstract class Word extends Expr

case class Terminal(val s: String) extends Word {
  def format = s
}

case class Nonterminal(val name: Symbol) extends Word {
  def format = "<" + name.name + ">"

  def ::=(rule: Expr): Production = new Production(this, rule)
}

case class Sequence(val left: Expr, val right: Expr) extends Expr {
  def format =
    Seq(left, right).map {
      case a: Alternation => "(" + a.format + ")"
      case other => other.format
    }.mkString
}

case class Alternation(val left: Expr, val right: Expr) extends Expr {
  def format = left.format + "|" + right.format
}

case class Repetition(val expr: Expr) extends Expr {
  def format = "{" + expr.format + "}"
}

case class Option(val expr: Expr) extends Expr {
  def format = "[" + expr.format + "]"
}

case object ε extends Expr {
  def format = "ε"
}

object ExprImplicts {
  implicit def charToTerminal(s: String) = Terminal(s)
  implicit def symbolToNonterminal(s: Symbol) = Nonterminal(s)
}

// <nt> ::= rule
class Production(val nt: Nonterminal, val rule: Expr) {
  override def toString() = s"Production($nt, $rule)"

  def format = nt.format + " ::= " + rule.format
}

class Grammar(val rules: Seq[Production]) {
  override def toString() = s"Grammar($rules)"

  def format =
    rules.map(_.format).mkString(" ;\n") match {
      case "" => "" // empty
      case s: String => s + " ;"
    }

  def nonterminals: Set[Nonterminal] = rules.map(_.nt).toSet
}

// object Main extends App {
//   val e: Expr = ("a" ~ "b").*
//   val f: Expr = "a".?
//   val p: Production = 'A ::= "a".?
//   println(e,f,p)
// }
