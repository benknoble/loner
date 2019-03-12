package org.benknoble.ebnf

import org.scalatest._

class GrammarSpec extends FlatSpec with Matchers {

  import ExprImplicts._

  val emptyGrammar = Grammar(List())
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
  val G = new Grammar(Seq(
    Pa,
    Pempty,
    `Pa*`,
    `P1(a)*b`,
    `P1(a)?b`,
    `P1(a|c)b`))

  "The empty Grammar object" should "have an empty string representation" in {
    emptyGrammar.toString() shouldEqual ""
  }

  "A Terminal of a string" should "be represented by that string" in {
    Terminal("a").toString() shouldEqual "a"
    Terminal("ab").toString() shouldEqual "ab"
  }

  "A Nonterminal of a string" should "be represented by the string" in {
    abc.toString() shouldEqual "<abc>"
  }

  "A Sequence" should "be the concatentation of its elements" in {
    (`a*` ~ ε).toString() shouldEqual "{a}ε"
    `1(a)*b`.toString() shouldEqual "1{a}b"
    `1(a)?b`.toString() shouldEqual "1[a]b"
    `1(a|c)b`.toString() shouldEqual "1(a|c)b"
  }

  "An Alternation" should "consist of |-separated elements" in {
    (`a*` || ε).toString() shouldEqual "{a}|ε"
    `(1|(a)*|b)`.toString() shouldEqual "1|{a}|b"
    `(1|(a)?|b)`.toString() shouldEqual "1|[a]|b"
    `(1|(a|c)|b)`.toString() shouldEqual "1|a|c|b"
  }

  "A Repetition" should "be surrounded by {}" in {
    `a*`.toString shouldEqual "{a}"
  }

  "An Option" should "be surrounded by []" in {
    `a?`.toString() shouldEqual "[a]"
  }

  "Epsilon" should "be ε" in {
    ε.toString() shouldEqual "ε"
  }

  "A Production" should "be <Nonterminal> ::= rule(s)" in {
    Pa.toString shouldEqual "<A> ::= a"
    Pempty.toString() shouldEqual "<A> ::= ε"
    `Pa*`.toString() shouldEqual "<A> ::= {a}"
    `P1(a)*b`.toString() shouldEqual "<A> ::= 1{a}b"
    `P1(a)?b`.toString() shouldEqual "<A> ::= 1[a]b"
    `P1(a|c)b`.toString() shouldEqual "<A> ::= 1(a|c)b"
  }

  "A Grammar" should "be newline-separated Productions" in {
    G.toString() shouldEqual """<A> ::= a
<A> ::= ε
<A> ::= {a}
<A> ::= 1{a}b
<A> ::= 1[a]b
<A> ::= 1(a|c)b"""
  }

  "A Grammar's nonterminals" should "be the set of nonterminals" in {
    G.nonterminals shouldEqual Set[Nonterminal]('A)
  }
}
