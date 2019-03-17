package org.benknoble.ebnf

import scala.language.implicitConversions

import ExprImplicts._

abstract class Expr {
  def ~ (right: Expr): Expr = Sequence(this, right)
  def || (right: Expr): Expr = Alternation(this, right)
  def * = Repetition(this)
  def ? = Option(this)
}

abstract class Word extends Expr

case class Terminal(val s: String) extends Word {
  override def toString() = s
}

case class Nonterminal(val name: Symbol) extends Word {
  override def toString() = "<" + name.name + ">"

  def ::=(rule: Expr) = new Production(this, rule)
}

case class Sequence(val left: Expr, val right: Expr) extends Expr {
  override def toString() =
    Util.join(
      "",
      Seq(left, right).map {
        case a: Alternation => "(" + a + ")"
        case other => other.toString()
      })
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
  override def toString() = {
    val joined = Util.join(" ;\n", rules)
    if (joined.isEmpty)
      joined
    else
      joined + " ;"
  }

  def nonterminals: Set[Nonterminal] = rules.map(_.nt).toSet
}

// object Main extends App {
//   val e: Expr = ("a" ~ "b").*
//   val f: Expr = "a".?
//   val p: Production = 'A ::= "a".?
//   println(e,f,p)
// }
