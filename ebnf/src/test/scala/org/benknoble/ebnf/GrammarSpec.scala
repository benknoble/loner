package org.benknoble.ebnf

import org.scalatest._

class GrammarSpec extends FlatSpec with Matchers {

  val emptyGrammar = Grammar(List())
  val abc: Nonterminal = "abc"
  val emptySeq = Sequence(List())
  val emptyBranch = Alternation(List())
  val `a*` = Repetition('a')
  val `a?` = Option('a')
  val `1(a)*b` = Sequence(List('1', `a*`, 'b'))
  val `1(a)?b` = Sequence(List('1', `a?`, 'b'))
  val `1(a|c)b` = Sequence(List('1', Alternation(List('a', 'c')), 'b'))
  val `(1|(a)*|b)` = Alternation(List('1', `a*`, 'b'))
  val `(1|(a)?|b)` = Alternation(List('1', `a?`, 'b'))
  val `(1|(a|c)|b)` = Alternation(List('1', Alternation(List('a', 'c')), 'b'))
  val Ra = Rule('a')
  val Rempty = Rule(emptySeq)
  val `Ra*` = Rule(Sequence(List(`a*`)))
  val `R1(a)*b` = Rule(`1(a)*b`)
  val `R1(a)?b` = Rule(`1(a)?b`)
  val `R1(a|c)b` = Rule(`1(a|c)b`)
  val Pa = Production("A", Ra)
  val Pempty = Production("A", Rempty)
  val `Pa*` = Production("A", `Ra*`)
  val `P1(a)*b` = Production("A", `R1(a)*b`)
  val `P1(a)?b` = Production("A", `R1(a)?b`)
  val `P1(a|c)b` = Production("A", `R1(a|c)b`)
  val G = Grammar(List(
    Pa,
    Pempty,
    `Pa*`,
    `P1(a)*b`,
    `P1(a)?b`,
    `P1(a|c)b`))

  "The empty Grammar object" should "have an empty string representation" in {
    emptyGrammar.toString() shouldEqual ""
  }

  "A Terminal of a character" should "be represented by that character" in {
    Terminal('a').toString() shouldEqual "a"
  }

  "A Nonterminal of a string" should "be represented by the string" in {
    abc.toString() shouldEqual "<abc>"
  }

  "A Sequence" should "be the concatentation of its elements" in {
    emptySeq.toString() shouldEqual "ε"
    Sequence(List(`a*`)).toString() shouldEqual "(a)*"
    `1(a)*b`.toString() shouldEqual "1(a)*b"
    `1(a)?b`.toString() shouldEqual "1(a)?b"
    `1(a|c)b`.toString() shouldEqual "1(a|c)b"
  }

  "An Alternation" should "be surrounded in () and consist of |-separated elements" in {
    emptyBranch.toString() shouldEqual "ε"
    Alternation(List(`a*`)).toString() shouldEqual "((a)*)"
    `(1|(a)*|b)`.toString() shouldEqual "(1|(a)*|b)"
    `(1|(a)?|b)`.toString() shouldEqual "(1|(a)?|b)"
    `(1|(a|c)|b)`.toString() shouldEqual "(1|(a|c)|b)"
  }

  "A Repetition" should "be surrounded by ()*" in {
    `a*`.toString shouldEqual "(a)*"
  }

  "An Option" should "be surrounded by ()?" in {
    `a?`.toString() shouldEqual "(a)?"
  }

  "Epsilon" should "be ε" in {
    Expr.ε.toString() shouldEqual "ε"
  }

  "A Rule" should "be its expression" in {
    Ra.toString() shouldEqual "a"
    Rempty.toString() shouldEqual "ε"
    `Ra*`.toString() shouldEqual "(a)*"
    `R1(a)*b`.toString() shouldEqual "1(a)*b"
    `R1(a)?b`.toString() shouldEqual "1(a)?b"
    `R1(a|c)b`.toString() shouldEqual "1(a|c)b"
  }

  "A Production" should "be <Nonterminal> ::= rule(s)" in {
    Pa.toString shouldEqual "<A> ::= a"
    Pempty.toString() shouldEqual "<A> ::= ε"
    `Pa*`.toString() shouldEqual "<A> ::= (a)*"
    `P1(a)*b`.toString() shouldEqual "<A> ::= 1(a)*b"
    `P1(a)?b`.toString() shouldEqual "<A> ::= 1(a)?b"
    `P1(a|c)b`.toString() shouldEqual "<A> ::= 1(a|c)b"
  }

  "A Grammar" should "be newline-separated Productions" in {
    G.toString() shouldEqual """<A> ::= a
<A> ::= ε
<A> ::= (a)*
<A> ::= 1(a)*b
<A> ::= 1(a)?b
<A> ::= 1(a|c)b"""
  }

  "A Grammar's nonterminals" should "be the set of nonterminals" in {
    G.nonterminals shouldEqual Set[Nonterminal]("A")
  }
}
