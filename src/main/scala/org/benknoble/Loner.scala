package org.benknoble.loner

import org.benknoble.ebnf._

object Loner {

  import ExprImplicits._

  def ⊙ = SetFilter[Word](ε)

  type NtMap[A] = Map[Nonterminal, A]
  type Nullables = NtMap[Boolean]
  type Starters = NtMap[Set[Word]]
  type Followers = NtMap[Set[Word]]

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

  private def N(prev: Nullables)(e: Expr): Boolean = e match {
    case `ε` => true
    case Terminal(_) => false
    case n: Nonterminal => prev(n)
    case Sequence(left, right) => N(prev)(left) && N(prev)(right)
    case Alternation(left, right) => N(prev)(left) || N(prev)(right)
    case Repetition(_) => true
    case Option(_) => true
  }

  private def nullable_i(g: Grammar, prev: Nullables): Nullables = {
    mapGrammar(N(prev))(g)
  }

  def nullable(g: Grammar): Nullables = {
    val n0 = g.nonterminals.map(_ -> false).toMap
    fix(nullable_i)(n0)(g)
  }

  private def S(prev: Starters)(e: Expr): Set[Word] = e match {
    case `ε` => Set(ε)
    case t: Terminal => Set(t)
    case n: Nonterminal => prev(n)
    case Sequence(left, right) => ⊙(S(prev)(left))(S(prev)(right))
    case Alternation(left, right) => S(prev)(left) union S(prev)(right)
    case Repetition(exp) => S(prev)(exp) union Set(ε)
    case Option(exp) => S(prev)(exp) union Set(ε)
  }

  private def starters_i(g: Grammar, prev: Starters): Starters =
    mapGrammar(S(prev))(g)


  def starters(g: Grammar): Starters = {
    val nullmap = nullable(g)
    val s0 = g.nonterminals.map(n => {
      val value: Set[Word] = if (nullmap(n)) Set(ε) else Set.empty
      n -> value
    }).toMap
    fix(starters_i)(s0)(g)
  }

  private def contains(n: Nonterminal)(e: Expr): Boolean = e match {
    case `n` => true
    case w: Word => false
    case Sequence(left, right) => contains(n)(left) || contains(n)(right)
    case Alternation(left, right) => contains(n)(left) || contains(n)(right)
    case Repetition(exp) => contains(n)(exp)
    case Option(exp) => contains(n)(exp)
  }

  // should this go in Expr?
  // should any of these utility methods?
  private def sequify(e: Expr): Seq[Expr] = e match {
    case w: Word => Seq(w)
    case Alternation(left, right) => sequify(left) ++ sequify(right)
    case Sequence(left, right) => sequify(left) ++ sequify(right)
    case Repetition(exp) => sequify(exp)
    case Option(exp) => sequify(exp)
  }

  private def sequify_alternation(e: Expr): Seq[Expr] = e match {
    case w: Word => Seq(w)
    case Alternation(left, right) => {
      def alt(e: Expr) = e match {
        case a: Alternation => sequify_alternation(a)
        case _ => Seq(e)
      }
      val lef = alt(left)
      val rig = alt(right)
      lef ++ rig
    }
    case Sequence(left, right) => sequify_alternation(left) ++ sequify_alternation(right)
    case Repetition(exp) => sequify_alternation(exp)
    case Option(exp) => sequify_alternation(exp)
  }

  private def after(n: Nonterminal)(e: Expr): Seq[Expr] = e match {
    case w: Word => Seq()
    case Alternation(left, right) => after(n)(left) ++ after(n)(right)
    case Repetition(exp) => after(n)(exp) ++ (
      if (contains(n)(exp))
        Seq(n)
      else
        Seq())
    case Option(exp) => after(n)(exp)
    case Sequence(left, right) => {
      val lef =
        if (contains(n)(left))
          after(n)(left) ++ sequify(right)
        else
          Seq()

      val rig =
        if (contains(n)(right))
          after(n)(right)
        else
          Seq()

      lef ++ rig
    }
  }

  def followers(g: Grammar): Followers = {
    val nullmap = nullable(g)
    val startmap = starters(g)
    val Sf = S(startmap)(_)
    val Nf = N(nullmap)(_)

    def F0(n: Nonterminal)(e: Expr): Seq[Set[Word]] = e match {
      case w: Word => Seq(Set.empty)
      case Alternation(left, right) => F0(n)(left) ++ F0(n)(right)
      case Repetition(exp) => F0(n)(exp)
      case Option(exp) => F0(n)(exp)
      case s: Sequence if (contains(n)(s)) => {
        val starter_sets = after(n)(s)
          .map(Sf)
          .reduceOption(⊙(_)(_))
          .getOrElse(Set.empty)
        Seq(starter_sets)
      }
      // otherwise: n not in seq, so we don't need to recurse
      case _: Sequence => Seq()
    }

    val f0 = g.nonterminals.map(a => {
      val a_finder = F0(a)(_)
      val f0_a = g.rules
        .flatMap(p => a_finder(p.rule))
        .reduceOption(_ union _)
        .getOrElse(Set.empty)
      a -> (f0_a - ε)
    }).toMap

    def followers_i(g: Grammar, prev: Followers): Followers = {
      def F(n: Nonterminal)(p: Production): Seq[Set[Word]] = p.rule match {
        case `n` => Seq(prev(p.nt))
        case w: Word => Seq()
        case Alternation(left, right) => F(n)(p.nt ::= left) ++ F(n)(p.nt ::= right)
        case Repetition(exp) => F(n)(p.nt ::= exp)
        case Option(exp) => F(n)(p.nt ::= exp)
        case s: Sequence if (
          contains(n)(s) && after(n)(s).forall(Nf)) => Seq(prev(p.nt))
        case _: Sequence => Seq()
      }

      g.nonterminals.map(a => {
        val a_finder = F(a)(_)
        val fi_a = g.rules
          .flatMap(p => a_finder(p))
          .reduceOption(_ union _)
          .getOrElse(Set.empty)
          a -> (fi_a union prev(a))
      }).toMap
    }

    fix(followers_i)(f0)(g)
  }

  private def areDisjoint[A](sets: Set[A]*): Boolean =
    sets.reduceOption(_ intersect _).getOrElse(Set.empty) == Set.empty

  def isLLone(g: Grammar): Boolean = {
    val nullmap = nullable(g)
    val Nf = N(nullmap)(_)
    val startmap = starters(g)
    val Sf = S(startmap)(_)
    val follmap = followers(g)

    def predict(p: Production): Boolean = p.rule match {
      case Sequence(a: Alternation, right) => {
        val choices = sequify_alternation(a)
        val predictions = choices.map(c => ⊙(Sf(c ~ right))(follmap(p.nt)))

        (areDisjoint(predictions :_*)
          && predict(p.nt ::= a)
          && predict(p.nt ::= right))
      }
      case a: Alternation => {
        val choices = sequify_alternation(a)
        val predictions = choices.map(c => ⊙(Sf(c))(follmap(p.nt)))

        (areDisjoint(predictions :_*)
          && predict(p.nt ::= a.left)
          && predict(p.nt ::= a.right))
      }
      case Sequence(Repetition(exp), right) => {
        val disjoint = ((!Nf(exp))
          && areDisjoint(Sf(exp), ⊙(Sf(right))(follmap(p.nt))))

        (disjoint
          && predict(p.nt ::= exp)
          && predict(p.nt ::= right))
      }
      case Repetition(exp) => {
        val disjoint = (!Nf(exp)) && areDisjoint(Sf(exp), follmap(p.nt))

        (disjoint
          && predict(p.nt ::= exp))
      }
      case w: Word => true
      case Sequence(left, right) => predict(p.nt ::= left) && predict(p.nt ::= right)
      case Option(exp) => predict(p.nt ::= exp)
    }

    g.rules.map(p => predict(p)).forall(b => b)
  }

}
