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

object Expr {
  def reduceTree(es: Seq[Expr], f: (Expr, Expr) => Expr): Expr =
    es.tail.foldLeft(es.head)(f)

  def sequencify(exprs: Seq[Expr]): Expr = reduceTree(exprs, Sequence(_,_))
  def branchify(exprs: Seq[Expr]): Expr = reduceTree(exprs, Alternation(_,_))
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
  override def equals(that: Any) = that match {
    case that: Production => nt == that.nt && rule == that.rule
    case _ => false
  }

  def format = nt.format + " ::= " + rule.format
}

class Grammar(_rules: Seq[Production]) {
  override def toString() = s"Grammar($rules)"
  override def equals(that: Any) = that match {
    // order matters
    case that: Grammar => rules == that.rules
    case _ => false
  }

  def format =
    rules.map(_.format).mkString(" ;\n") match {
      case "" => "" // empty
      case s: String => s + " ;"
    }

  val rules: Seq[Production] =
    _rules
      .groupBy(_.nt)
      .map { case (nt: Nonterminal, ps: Seq[Production]) =>
        val rules_for_nt = ps.map(_.rule)
        nt ::= Expr.branchify(rules_for_nt)
      }.toList

  def nonterminals: Set[Nonterminal] = lhsNonterminals union rhsNonterminals

  private def lhsNonterminals: Set[Nonterminal] = rules.map(_.nt).toSet
  private def rhsNonterminals: Set[Nonterminal] = {
    def nonterminals(e: Expr): Set[Nonterminal] = e match {
      // backticks match against object ε
      case `ε` => Set()
      case Terminal(_) => Set()
      case n @ Nonterminal(_) => Set(n)
      case Sequence(left, right) => nonterminals(left) union nonterminals(right)
      case Alternation(left, right) => nonterminals(left) union nonterminals(right)
      case Repetition(e) => nonterminals(e)
      case Option(e) => nonterminals(e)
    }

    rules
      .map(_.rule)
      .foldLeft(Set[Nonterminal]())((acc, rule) => acc union nonterminals(rule))
  }
}

// object Main extends App {
//   val e: Expr = ("a" ~ "b").*
//   val f: Expr = "a".?
//   val p: Production = 'A ::= "a".?
//   println(e,f,p)
// }
