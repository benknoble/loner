package org.benknoble.loner.ebnf

import scala.language.implicitConversions

import ExprImplicts._

abstract class Expr {
  def ~ (right: Expr): Expr = Sequence(this, right)
  def || (right: Expr): Expr = Alternation(this, right)
  def *(): Expr = this match {
    case r: Repetition => this // (r*)* = r*
    case Option(e) => Repetition(e) // (o?)* = o*
    case _ => Repetition(this)
  }
  def ?(): Expr = this match {
    case r: Repetition => this // (r*)? = r*
    case o: Option => this // (o?)? = o?
    case _ => Option(this)
  }
}

abstract class Word extends Expr

case class Terminal(val s: String) extends Word {
  override def toString() = s
}

case class Nonterminal(val name: Symbol) extends Word {
  override def toString() = "<" + name.name + ">"

  def ::=(rule: Expr): Production = new Production(this, rule)
}

case class Sequence(val left: Expr, val right: Expr) extends Expr {
  override def toString() =
    Seq(left, right).map {
      case a: Alternation => "(" + a + ")"
      case other => other.toString()
    }.mkString
}

case class Alternation(val left: Expr, val right: Expr) extends Expr {
  override def toString() = left.toString() + "|" + right.toString()
}

case class Repetition(val expr: Expr) extends Expr {
  override def toString() = "{" + expr + "}"
}

case class Option(val expr: Expr) extends Expr {
  override def toString() = "[" + expr + "]"
}

case object ε extends Expr {
  override def toString() = "ε"
}

object ExprImplicts {
  implicit def charToTerminal(s: String) = Terminal(s)
  implicit def symbolToNonterminal(s: Symbol) = Nonterminal(s)
}

// <nt> ::= rule
class Production(val nt: Nonterminal, val rule: Expr) {
  override def toString() = nt.toString() + " ::= " + rule.toString()
}

class Grammar(val rules: Seq[Production]) {
  override def toString() =
    rules.mkString(" ;\n") match {
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
