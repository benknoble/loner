package org.benknoble.loner.ebnf

import org.scalatest._
import ExprImplicts._

class EbnfParserSpec extends FlatSpec with Matchers with EitherValues {

  "EbnfParser" should "parse an empty grammar" in {
    EbnfParser("").right.get shouldEqual Grammar()
  }

  it should "parse a simple single-rule grammar" in {
    EbnfParser("<A> ::= a ;").right.value shouldEqual
      Grammar('A ::= "a")
    EbnfParser("<A> ::= ε ;").right.value shouldEqual
      Grammar('A ::= ε)
  }

  it should "parse a multi-rule grammar with the same non-terminals" in {
    EbnfParser("<A> ::= a; <A> ::= b; ").right.value shouldEqual
      Grammar('A ::= "a" || "b")
  }

  it should "parse a single-rule grammar with sequences" in {
    EbnfParser("<A> ::= abc;").right.value shouldEqual
      Grammar('A ::= "abc")
    EbnfParser("<A> ::= a(bc);").right.value shouldEqual
      Grammar('A ::= "a" ~ "bc")
  }

  it should "parse a single-rule grammar with alternations" in {
    EbnfParser("<A> ::= a|b|c;").right.value shouldEqual
      Grammar('A ::= "a" || "b" || "c")
    EbnfParser("<A> ::= a|b[d]|c;").right.value shouldEqual
      Grammar('A ::= "a" || ("b" ~ "d".?) || "c")
  }

  it should "parse a single-rule grammar with repetitions" in {
    EbnfParser("<A> ::= {a};").right.value shouldEqual
      Grammar('A ::= Terminal("a").*)
  }

  it should "parse a single-rule grammar with options" in {
    EbnfParser("<A> ::= [a];").right.value shouldEqual
      Grammar('A ::= "a".?)
  }

  it should "parse a single-rule grammar with nonterminals" in {
    EbnfParser("<A> ::= <A>;").right.value shouldEqual
      Grammar('A ::= 'A)
  }

  it should "parse a single-rule grammar with all constructs" in {
    EbnfParser("<A> ::= a<A>([b]|c){d};").right.value shouldEqual
      Grammar('A ::= "a" ~ 'A ~ ("b".? || "c") ~ Terminal("d").*)
  }

  it should "parse a multi-rule grammar with all constructs" in {
    EbnfParser(
      "<A> ::= a<A>([b]|c){d};<B> ::= a<B>([b]|c){d};"
    ).right.value shouldEqual Grammar(
      'A ::= "a" ~ 'A ~ ("b".? || "c") ~ Terminal("d").*,
      'B ::= "a" ~ 'B ~ ("b".? || "c") ~ Terminal("d").*
    )
  }

  it should "fail on grammars missing semi-colons" in {
    EbnfParser("<A> ::= a") shouldBe a [Left[_,_]]
  }

  it should "fail on grammars not starting with non-terminals" in {
    EbnfParser("A ::= a;") shouldBe a [Left[_,_]]
    EbnfParser("A> ::= a;") shouldBe a [Left[_,_]]
    EbnfParser("<A ::= a;") shouldBe a [Left[_,_]]
  }

  it should "fail on grammars missing the 'goes-to' symbol '::='" in {
    EbnfParser("<A> := a;") shouldBe a [Left[_,_]]
    EbnfParser("<A> :: a;") shouldBe a [Left[_,_]]
  }

  it should "fail on grammars with no right-hand side" in {
    EbnfParser("<A> ::= ;") shouldBe a [Left[_,_]]
  }

  it should "ignore whitespace" in {
    EbnfParser("""
      <A>
        ::= a
        | b	[c    ]
        ;"""
        ).right.value shouldEqual
      Grammar('A ::= "a" || "b" ~ "c".?)
  }

}
