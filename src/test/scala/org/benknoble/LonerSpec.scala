package org.benknoble.loner

import org.scalatest._

class LonerSpec extends FlatSpec with Matchers {

  import org.benknoble.ebnf._
  import ExprImplicits._

  def unpack[A](map: Loner.NtMap[A]) =
    map.map { case (n, a) => n.name -> a }

  val example1 = Grammar(
    'S ::= 'A ~ "$",
    'A ::= 'B ~ 'D ~ 'A || "a",
    'B ::= 'D || "b",
    'D ::= "d" || ε
  )

  "The Loner object" should "compute nullable sets" in {
    unpack(Loner.nullable(example1)) shouldEqual Map(
      'S -> false,
      'A -> false,
      'B -> true,
      'D -> true
      )
  }

  it should "compute starter sets" in {
    unpack(Loner.starters(example1)) shouldEqual Map(
      'S -> Set[Word]("a", "b", "d"),
      'A -> Set[Word]("a", "b", "d"),
      'B -> Set[Word]("b", "d", ε),
      'D -> Set[Word]("d", ε)
      )
  }

  it should "compute follower sets" in {
    unpack(Loner.followers(example1)) shouldEqual Map(
      'S -> Set[Word](),
      'A -> Set[Word]("$"),
      'B -> Set[Word]("a", "b", "d"),
      'D -> Set[Word]("a", "b", "d")
    )
  }
}
