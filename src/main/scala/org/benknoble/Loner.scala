package org.benknoble.loner

import scala.scalajs.js.annotation.{ JSExportTopLevel, JSExport }

import org.benknoble.ebnf._

/** LL(1) computer
  *
  * Applicable to org.benknoble.ebnf.Grammar instances
  */
@JSExportTopLevel("Loner")
object Loner {

  import ExprImplicits._

  /** The monoid operation on Set[Word] defined by
    * [[org.benknoble.loner.SetFilter]]
    *
    * ⊙(S)(T) = if (S contains ε) (S - ε) union T else S
    */
  def ⊙ = SetFilter[Word](ε)

  /** Map from org.benknoble.ebnf.Nonterminal to 'A' */
  type NtMap[A] = Map[Nonterminal, A]
  /** Type of the nullable computation
    *
    * Map from org.benknoble.ebnf.Nonterminal to Boolean
    */
  type Nullables = NtMap[Boolean]
  /** Type of the starter computation
    *
    * Map from org.benknoble.ebnf.Nonterminal to Set[Word]
    */
  type Starters = NtMap[Set[Word]]
  /** Type of the follower computation
    *
    * Map from org.benknoble.ebnf.Nonterminal to Set[Word]
    */
  type Followers = NtMap[Set[Word]]

  /** Returns a function which computes the least fixed point, given a starting
    * value and a step function
    *
    * @param fi step function
    * @param f0 start value
    */
  def fix[A,B](fi: (A, B) => B)(f0: B): (A => B) = a => {
    @annotation.tailrec
    def go(prev: B, cur: B): B =
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

  /** Computes nullable map of an org.benknoble.ebnf.Grammar */
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


  /** Computes starter map of an org.benknoble.ebnf.Grammar */
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

  /** Computes follower map of an org.benknoble.ebnf.Grammar */
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

  /** true iff all sets are mutually disjoint
    *
    * Equivalent to ∩(sets) == ∅
    *
    * @param sets the sets to test
    */
  def areDisjoint[A](sets: Set[A]*): Boolean =
    sets.reduceOption(_ intersect _).getOrElse(Set.empty) == Set.empty

  /** true iff the org.benknoble.ebnf.Gramamr is in LL(1) */
  @JSExport
  def isLLone(g: Grammar): Boolean = {
    if (!g.isWellFormed)
      return false
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

  /** equivalent to Loner.isLLone(g) */
  def apply(g: Grammar): Boolean = isLLone(g)

}
