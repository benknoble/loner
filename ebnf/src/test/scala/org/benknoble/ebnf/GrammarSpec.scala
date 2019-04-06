package org.benknoble.ebnf

import org.scalatest._

class GrammarSpec extends FlatSpec with Matchers {

  import ExprImplicits._

  val emptyGrammar = Grammar()
  val abc: Nonterminal = 'abc
  val `a*`: Expr = Terminal("a").*
  val `a?`: Expr = "a".?
  val `1(a)*b`: Expr = "1" ~ `a*` ~ "b"
  val `1(a)?b`: Expr = "1" ~ `a?` ~ "b"
  val `1(a|c)b`: Expr = "1" ~ ("a" || "c") ~ "b"
  val `(1|(a)*|b)`: Expr = "1" || `a*` || "b"
  val `(1|(a)?|b)`: Expr = "1" || `a?` || "b"
  val `(1|(a|c)|b)`: Expr = "1" || ("a"||"c") || "b"
  val Pa = 'A ::= "a"
  val Pempty = 'A ::= ε
  val `Pa*` = 'A ::= `a*`
  val `P1(a)*b` = 'A ::= `1(a)*b`
  val `P1(a)?b` = 'A ::= `1(a)?b`
  val `P1(a|c)b` = 'A ::= `1(a|c)b`
  val G = Grammar(
    Pa,
    Pempty,
    `Pa*`,
    `P1(a)*b`,
    `P1(a)?b`,
    `P1(a|c)b`,
    'C ::= "c")

  "The empty Grammar object" should "have an empty string representation" in {
    emptyGrammar.format shouldEqual ""
  }

  "A Terminal of a string" should "be represented by that string" in {
    Terminal("a").format shouldEqual "a"
    Terminal("ab").format shouldEqual "ab"
  }

  "A Nonterminal of a string" should "be represented by the string" in {
    abc.format shouldEqual "<abc>"
  }

  "A Sequence" should "be the concatentation of its elements" in {
    (`a*` ~ ε).format shouldEqual "{a}ε"
    `1(a)*b`.format shouldEqual "1{a}b"
    `1(a)?b`.format shouldEqual "1[a]b"
    `1(a|c)b`.format shouldEqual "1(a|c)b"
  }

  "An Alternation" should "consist of |-separated elements" in {
    (`a*` || ε).format shouldEqual "{a}|ε"
    `(1|(a)*|b)`.format shouldEqual "1|{a}|b"
    `(1|(a)?|b)`.format shouldEqual "1|[a]|b"
    `(1|(a|c)|b)`.format shouldEqual "1|a|c|b"
  }

  "A Repetition" should "be surrounded by {}" in {
    `a*`.format shouldEqual "{a}"
  }

  "An Option" should "be surrounded by []" in {
    `a?`.format shouldEqual "[a]"
  }

  "Epsilon" should "be ε" in {
    ε.format shouldEqual "ε"
  }

  "A Production" should "be <Nonterminal> ::= rule(s)" in {
    Pa.format shouldEqual "<A> ::= a"
    Pempty.format shouldEqual "<A> ::= ε"
    `Pa*`.format shouldEqual "<A> ::= {a}"
    `P1(a)*b`.format shouldEqual "<A> ::= 1{a}b"
    `P1(a)?b`.format shouldEqual "<A> ::= 1[a]b"
    `P1(a|c)b`.format shouldEqual "<A> ::= 1(a|c)b"
  }

  "A Grammar" should "be ;-delimited, newline-separated Productions" in {
    G.format shouldEqual """<A> ::= a|ε|{a}|1{a}b|1[a]b|1(a|c)b ;
<C> ::= c ;"""
  }

  "A Grammar's nonterminals" should "be the set of nonterminals" in {
    G.nonterminals shouldEqual Set[Nonterminal]('A, 'C)
    new Grammar(G.rules ++ Seq[Production]('A ::= 'B))
      .nonterminals shouldEqual Set[Nonterminal]('A, 'B, 'C)
  }
}
