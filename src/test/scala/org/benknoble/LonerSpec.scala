package org.benknoble.loner

import org.scalatest._

class LonerSpec extends FlatSpec with Matchers {

  import org.benknoble.ebnf._
  import ExprImplicits._
  val example1 = Grammar(
    'S ::= 'A ~ "$",
    'A ::= 'B ~ 'D ~ 'A || "a",
    'B ::= 'D || "b",
    'D ::= "d" || ε
  )

  "The Loner object" should "compute nullable sets" in {
    Loner.nullable(example1) shouldEqual Map(
      'S -> false,
      'A -> false,
      'B -> true,
      'D -> true
      )
  }
}
