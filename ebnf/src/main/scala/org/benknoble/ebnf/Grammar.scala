package org.benknoble.ebnf

abstract class Expr

abstract class Word extends Expr

case class Terminal(c: Char) extends Word {
  override def toString() = c.toString()
}

case class Nonterminal(name: String) extends Word {
  override def toString() = "<" + name + ">"
}

case class Sequence(words: List[Expr]) extends Expr {
  override def toString() = if (words.isEmpty)
    "ε"
  else
    words.foldLeft("")(_.toString() + _.toString())
}

case class Alternation(branches: List[Expr]) extends Expr {
  override def toString() = if (branches.isEmpty)
    "ε"
  else
    "(" + Util.join("|", branches) + ")"
}

case class Repetition(expr: Expr) extends Expr {
  override def toString() = "(" + expr + ")*"
}

case class Option(expr: Expr) extends Expr {
  override def toString() = "(" + expr + ")?"
}

object Expr {
  def ε = Terminal('ε')

  implicit def termFromChar(c: Char) = Terminal(c)
  implicit def nontermFromString(s: String) = Nonterminal(s)
}

case class Rule(expr: Expr) {
  override def toString() = expr.toString()
}

// <nt> ::= rule
case class Production(nt: Nonterminal, rule: Rule) {
  override def toString() = nt.toString() + " ::= " + rule.toString()
}

case class Grammar(rules: List[Production]) {
  override def toString() = Util.join("\n", rules)
  def nonterminals = rules.map(_.nt).toSet
}
