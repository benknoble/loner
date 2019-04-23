package org.benknoble.ebnf

import scala.language.implicitConversions

import ExprImplicits._

/** Base class for expressions
  *
  * An expression is the right hand side of an EBNF rule, made of sequences,
  * branches, repetitions, and options of terminal and nonterminal symbols.
  * Each of these is represented by a concrete case class.
  *
  * Provides methods common to all expressions.
  */
abstract class Expr {
  /** Returns a [[org.benknoble.ebnf.Sequence]]
    *
    * @param right the next item in the sequence
    */
  def ~ (right: Expr): Expr = Sequence(this, right)

  /** Returns an [[org.benknoble.ebnf.Alternation]]
    *
    * @param right the right side of the branch
    */
  def || (right: Expr): Expr = Alternation(this, right)

  /** Returns a [[org.benknoble.ebnf.Repetition]] of this */
  def *(): Expr = Repetition(this)

  /** Returns a [[org.benknoble.ebnf.Option]] of this */
  def ?(): Expr = Option(this)

  /** Returns a formatted version of this
    *
    * Sub-classes implement this to determine formatting.
    *
    *  @return a print-able version in EBNF format
    */
  def format: String
}

/** Provides helper methods for expressions
  *
  * The most notable is `reduceTree`, which helps convert a Seq of
  * [[org.benknoble.ebnf.Expr]]s to an [[org.benknoble.ebnf.Expr]]
  * subtype representing an expression tree.
  *
  * Simple names are provided for the most common tree reductions.
  *
  * Used primarly by [[org.benknoble.ebnf.EbnfParser]] to map parser
  * results into trees.
  */
object Expr {
  /** Converts a series of expressions to a single expression tree
    *
    * Example:
    * {{{
    * Expr.reduceTree(List(Terminal("a"), Nonterminal("b")), Sequence(_,_))
    * }}}
    *
    * @param es sequence of expressions
    * @param f function to fold expressions together
    * @return a single [[org.benknoble.ebnf.Expr]] tree
    */
  def reduceTree(es: Seq[Expr], f: (Expr, Expr) => Expr): Expr =
    es.tail.foldLeft(es.head)(f)

  /** Convert a series of of expressions into a tree of
    * [[org.benknoble.ebnf.Sequence]]s
    */
  def sequencify(exprs: Seq[Expr]): Expr = reduceTree(exprs, Sequence(_,_))

  /** Convert a series of of expressions into a tree of
    * [[org.benknoble.ebnf.Alternation]]s
    */
  def branchify(exprs: Seq[Expr]): Expr = reduceTree(exprs, Alternation(_,_))
}

abstract class Word extends Expr

/** A terminal symbol of the grammar
  *
  * Terminals look like themselves:
  * {{{
  * scala> import org.benknoble.ebnf._
  * import org.benknoble.ebnf._
  *
  * scala> val t = Terminal("abc").format
  * t: String = abc
  * }}}
  *
  * @constructor create a new Terminal
  * @param s the String representing the terminal symbol
  */
case class Terminal(val s: String) extends Word {
  def format = s
}

/** A nonterminal symbol of the grammar
  *
  * Nonterminals look like `<Name>`:
  * {{{
  * scala> import org.benknoble.ebnf._
  * import org.benknoble.ebnf._
  *
  * scala> val n = Nonterminal('A).format
  * n: String = <A>
  * }}}
  *
  * @constructor create a new Nonterminal with a name
  * @param name the name of the Nonterminal
  */
case class Nonterminal(val name: Symbol) extends Word {
  def format = "<" + name.name + ">"

  /** Syntactic sugar to create [[org.benknoble.ebnf.Production]]s:
    * {{{
    * scala> import org.benknoble.ebnf._
    * import org.benknoble.ebnf._
    *
    * scala> val p = Nonterminal('A) ::= Terminal("a")
    * p: org.benknoble.ebnf.Production = Production(Nonterminal('A), Terminal(a))
    * }}}
    *
    * @param rule the right hand side of a production
    * @return an [[org.benknoble.ebnf.Production]] of this ::= rule
    */
  def ::=(rule: Expr): Production = new Production(this, rule)
}

/** A sequence of expressions in the tree
  *
  * Sequences are just their elements concatenated. They take special care to
  * group [[org.benknoble.ebnf.Alternation]]s in parens:
  * {{{
  * scala> import org.benknoble.ebnf._
  * import org.benknoble.ebnf._
  *
  * scala> val s = Sequence(Nonterminal('A), Terminal("a")).format
  * s: String = <A>a
  *
  * scala> val sa = Sequence(Nonterminal('A), Alternation(Terminal("a"), Terminal("b"))).format
  * sa: String = <A>(a|b)
  * }}}
  *
  * @constructor create a new Sequence from left to right
  * @param left the left side of the sequence
  * @param right the right side in the sequence
  */
case class Sequence(val left: Expr, val right: Expr) extends Expr {
  def format =
    Seq(left, right).map {
      case a: Alternation => "(" + a.format + ")"
      case other => other.format
    }.mkString
}

/** A choice of expressions in the tree
  *
  * Alternations are just their elements separated by '|':
  * {{{
  * scala> import org.benknoble.ebnf._
  * import org.benknoble.ebnf._
  *
  * scala> val a = Alternation(Nonterminal('A), Terminal("a")).format
  * a: String = <A>|a
  * }}}
  *
  * @constructor create a new Alternation from left to right
  * @param left the left side of the Alternation
  * @param right the right side in the Alternation
  */
case class Alternation(val left: Expr, val right: Expr) extends Expr {
  def format = left.format + "|" + right.format
}

/** The Kleene-star closure of an expression
  *
  * Repetitions are an expression surrounded by curly braces:
  * {{{
  * scala> val r = Repetition(Sequence(Nonterminal('A), Terminal("abc"))).format
  * r: String = {<A>abc}
  *
  * scala> val r2 = Sequence(Nonterminal('A), Terminal("abc")).*.format
  * r2: String = {<A>abc}
  * }}}
  *
  * @constructor create a new Repetition of expr
  * @param expr the expression to repeat
  */
case class Repetition(val expr: Expr) extends Expr {
  def format = "{" + expr.format + "}"
}

/** An optional expression.
  *
  * Equivalent to `Alternation(expr, ε)`. Surrounded by square brackets:
  * {{{
  * scala> var o = Option(Sequence(Nonterminal('A), Terminal("abc"))).format
  * o: String = [<A>abc]
  *
  * scala> o = Sequence(Nonterminal('A), Terminal("abc")).?.format
  * o: String = [<A>abc]
  * }}}
  *
  * @see [[org.benknoble.ebnf.Alternation]]
  * @see [[org.benknoble.ebnf.ε]]
  *
  * @constructor create a new Option of expr
  * @param expr the optional expression
  */
case class Option(val expr: Expr) extends Expr {
  def format = "[" + expr.format + "]"
}

/** The empty expression (pronounced "epsilon") */
case object ε extends Expr {
  def format = "ε"
}

/** Implicit conversions for Expr
  *
  * Creates a DSL-like system, with Strings automatically converted to
  * [[org.benknoble.ebnf.Terminal]]s and Symbols converted to
  * [[org.benknoble.ebnf.Nonterminal]]:
  * {{{
  * scala> import org.benknoble.ebnf._
  * import org.benknoble.ebnf._
  *
  * scala> import ExprImplicits._
  * import ExprImplicits._
  *
  * scala> val p = 'A ::= ("abc" || 'B.?) ~ 'C.*
  * p: org.benknoble.ebnf.Production = Production(Nonterminal('A), Sequence(Alternation(Terminal(abc),Option(Nonterminal('B))),Repetition(Nonterminal('C))))

  * scala> val f = p.format
  * f: String = <A> ::= (abc|[<B>]){<C>}
  * }}}
  *
  * Do note that `val r: Expr = "abc".*` will not work, due to ambiguity:
  * {{{
  * scala> val r: Expr = "abc".*
  * <console>:17: error: type mismatch;
  *  found   : String("abc")
  *  required: ?{def *: ?}
  * Note that implicit conversions are not applicable because they are ambiguous:
  *  both method augmentString in object Predef of type (x: String)scala.collection.immutable.StringOps
  *  and method charToTerminal in object ExprImplicits of type (s: String)org.benknoble.ebnf.Terminal
  *  are possible conversion functions from String("abc") to ?{def *: ?}
  *        val r: Expr = "abc".*
  *                      ^
  * <console>:17: error: value * is not a member of String
  *        val r: Expr = "abc".*
  *                            ^
  * }}}
  */
object ExprImplicits {
  implicit def charToTerminal(s: String) = Terminal(s)
  implicit def symbolToNonterminal(s: Symbol) = Nonterminal(s)
}

/** A production from a nonterminal to a rule
  *
  * A production is represented as `<Nonterminal> ::= expr`:
  * {{{
  * scala> val e = new Production(Nonterminal('A),Terminal("abc")).format
  * e: String = <A> ::= abc
  * }}}
  *
  * Contains a naïve comparison for equality based on the nonterminal and
  * expression (structural equality: `'A ::= "a"||"b"` does NOT equal
  * `'A ::= "b"||"a"`)
  *
  * @see [[org.benknoble.ebnf.Nonterminal]]'s ::= syntax
  * @see [[org.benknoble.ebnf.Expr]]
  *
  * @constructor create a new Production
  * @param nt the Nonterminal
  * @param rule the Expr
  */
class Production(val nt: Nonterminal, val rule: Expr) {
  override def toString() = s"Production($nt, $rule)"
  override def equals(that: Any) = that match {
    case that: Production => nt == that.nt && rule == that.rule
    case _ => false
  }

  /** Returns a formatted version of this */
  def format = nt.format + " ::= " + rule.format
}

/** A (context-free) grammar of productions
  *
  * The preferred construction method is via the companion object's apply
  * method, which is variadic.
  *
  * A Grammar is represented by ;-delimited, newline-separated Productions:
  * {{{
  * scala> val g = new Grammar(Seq(new Production(Nonterminal('A), Alternation(Terminal("abc"), Option(Nonterminal('B)))))).format
  * g: String = <A> ::= abc|[<B>] ;
  * }}}
  *
  * @see [[org.benknoble.ebnf.Production]]
  * @constructor create a new Grammar
  * @param _rules the sequence of Productions
  */
class Grammar(_rules: Seq[Production]) {
  override def toString() = s"Grammar($rules)"
  override def equals(that: Any) = that match {
    // order matters
    case that: Grammar => rules == that.rules
    case _ => false
  }

  /** Returns a formatted version of this */
  def format =
    rules.map(_.format).mkString(" ;\n") match {
      case "" => "" // empty
      case s: String => s + " ;"
    }

  /** The rules of the Grammar
    *
    * Any original rules with the same Nonterminal left-hand side are collapsed
    * into alternations:
    * {{{
    * scala> val g = Grammar('A ::= "abc", 'A ::= "def").format
    * g: String = <A> ::= abc|def ;
    * }}}
    *
    * Rather than expose the original constructor parameter, we apply a grouping
    * and then branchify the groups in order to create a "simplified" (but more
    * manageable) grammar.
    */
  val rules: Seq[Production] =
    _rules
      .groupBy(_.nt)
      .map { case (nt: Nonterminal, ps: Seq[Production]) =>
        val rules_for_nt = ps.map(_.rule)
        nt ::= Expr.branchify(rules_for_nt)
      }.toList

  /** A set of nonterminals present in the grammar */
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

/** Factory for [[org.benknoble.ebnf.Grammar]] instances. */
object Grammar {
  /** Creates a Grammar with the given productions
    *
    * @param productions the productions
    */
  def apply(productions: Production*): Grammar = new Grammar(productions)
}

// object Main extends App {
//   val e: Expr = ("a" ~ "b").*
//   val f: Expr = "a".?
//   val p: Production = 'A ::= "a".?
//   println(e,f,p)
// }
