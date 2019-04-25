package org.benknoble.loner

import org.benknoble.ebnf._

object Loner {

  def ⊙ = SetFilter[Word](ε)

  type NtMap[A] = Map[Nonterminal, A]
  type Nullable = NtMap[Boolean]
  type Starter = NtMap[Set[Word]]
  type Follower = NtMap[Set[Word]]

  def fix[A,B,C](fi: (A, Map[B, C]) => Map[B, C])(f0: Map[B, C]): (A => Map[B, C]) = a => {
    @annotation.tailrec
    def go(prev: Map[B, C], cur: Map[B, C]): Map[B, C] =
      if (prev == cur)
        cur
      else
        go(cur, fi(a, cur))

    go(f0, fi(a, f0))
  }

  private def mapGrammar[A](f: Expr => A)(g: Grammar): NtMap[A] =
    g.rules.map { p => p.nt -> f(p.rule) }.toMap

  private def nullable_i(g: Grammar, prev: Nullable): Nullable = {
    def N(e: Expr): Boolean = e match {
      case `ε` => true
      case Terminal(_) => false
      case n: Nonterminal => prev(n)
      case Sequence(left, right) => N(left) && N(right)
      case Alternation(left, right) => N(left) || N(right)
      case Repetition(_) => true
      case Option(_) => true
    }

    mapGrammar(N)(g)
  }

  def nullable(g: Grammar): Nullable = {
    val n0 = g.nonterminals.map(_ -> false).toMap
    fix(nullable_i)(n0)(g)
  }

  private def S(base: Starter)(e: Expr): Set[Word] = e match {
    case `ε` => Set(ε)
    case t: Terminal => Set(t)
    case n: Nonterminal => base(n)
    case Sequence(left, right) => ⊙(S(base)(left))(S(base)(right))
    case Alternation(left, right) => S(base)(left) union S(base)(right)
    case Repetition(exp) => S(base)(exp) union Set(ε)
    case Option(exp) => S(base)(exp) union Set(ε)
  }

  private def starters_i(g: Grammar, prev: Starter): Starter =
    mapGrammar(S(prev))(g)


  def starters(g: Grammar): Starter = {
    val nullmap = nullable(g)
    val s0 = g.nonterminals.map(n => {
      val value: Set[Word] = if (nullmap(n)) Set(ε) else Set()
      n -> value
    }).toMap
    fix(starters_i)(s0)(g)
  }

  def followers(g: Grammar): Follower = {
    val nullmap = nullable(g)
    val startmap = starters(g)
    val Sf = S(startmap)(_)

    def F(n: Nonterminal)(e: Expr): Seq[Set[Word]] = e match {
      // these cases should all be handled by the very specific Seq
      // implementation
      // case Sequence(`n`, right) => Seq(Sf(right))
      // case Sequence(_, Sequence(`n`, right)) => Seq(Sf(right))
      // case Sequence(Sequence(_, `n`), right) => Seq(Sf(right))
      case w: Word => Seq(Set())
      case Sequence(left, right) => F(n)(left) ++ F(n)(right)
      case Alternation(left, right) => F(n)(left) ++ F(n)(right)
      case Repetition(exp) => F(n)(exp)
      case Option(exp) => F(n)(exp)
    }

    val f0 = g.nonterminals.map(a => {
      val a_finder = F(a)(_)
      val f0_a = g.rules.flatMap(p => a_finder(p.rule)).reduce(_ union _)
      a -> (f0_a - ε)
    }).toMap

    f0
  }

  def isLLone(g: Grammar): Boolean = false

}
