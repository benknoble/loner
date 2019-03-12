package org.benknoble.ebnf

import ExprImplicts._

abstract class Expr {
  def ~ (right: Expr): Expr = Sequence(this, right)
  def || (right: Expr): Expr = Alternation(this, right)
  def * = Repetition(this)
  def ? = Option(this)
}

abstract class Word extends Expr

case class Terminal(s: String) extends Word {
  override def toString() = s

}

case class Nonterminal(name: Symbol) extends Word {
  override def toString() = "<" + name.name + ">"

  def ::=(rule: Expr) = new Production(this, rule)
}

case class Sequence(left: Expr, right: Expr) extends Expr {
  override def toString() = {
    def paren(alt: Alternation) = "(" + alt + ")"
    Util.join(
      "",
      Seq(left, right).map {
        case a: Alternation => paren(a)
        case other => other.toString()
      })
  }
}

case class Alternation(left: Expr, right: Expr) extends Expr {
  override def toString() = left.toString() + "|" + right.toString()
}

case class Repetition(expr: Expr) extends Expr {
  override def toString() = "{" + expr + "}"
}

case class Option(expr: Expr) extends Expr {
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
case class Production(nt: Nonterminal, rule: Expr) {
  override def toString() = nt.toString() + " ::= " + rule.toString()
}

case class Grammar(rules: List[Production]) {
  override def toString() = Util.join("\n", rules)
  def nonterminals = rules.map(_.nt).toSet
}

// object Main extends App {
//   val e: Expr = ("a" ~ "b").*
//   val f: Expr = "a".?
//   val p: Production = 'A ::= "a".?
//   println(e,f,p)
// }
