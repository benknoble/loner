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
    'D ::= "d" || ε)
  val example2 = Grammar(
    'S ::= 'A ~ 'B ~ "c",
    'A ::= 'A ~ "b" || ε,
    'B ::= "b" || ε)
  val example3 = Grammar(
    'S ::= 'A ~ "s",
    'A ::= 'B ~ "c" ~ 'A || ε,
    'B ::= ("b" ~ 'B) || ("a" ~ 'A))
  val example4 = Grammar(
    'S ::= 'A ~ "c",
    'A ::= ("a" ~ 'A.* ~ "b") || "b")
  val example5 = Grammar(
    'S ::= 'A ~ "$",
    'A ::= ("a" ~ 'A ~ "x") || 'B,
    'B ::= ("b" ~ 'B ~ "x") || 'D,
    'D ::= ("d" ~ 'D ~ "x") || "f")

  "The Loner object" should "compute nullable sets" in {
    unpack(Loner.nullable(example1)) shouldEqual Map(
      'S -> false,
      'A -> false,
      'B -> true,
      'D -> true)
    unpack(Loner.nullable(example2)) shouldEqual Map(
      'S -> false,
      'A -> true,
      'B -> true)
    unpack(Loner.nullable(example3)) shouldEqual Map(
      'S -> false,
      'A -> true,
      'B -> false)
    unpack(Loner.nullable(example4)) shouldEqual Map(
      'S -> false,
      'A -> false)
    unpack(Loner.nullable(example5)) shouldEqual Map(
      'S -> false,
      'A -> false,
      'B -> false,
      'D -> false)
  }

  it should "compute starter sets" in {
    unpack(Loner.starters(example1)) shouldEqual Map(
      'S -> Set[Word]("a", "b", "d"),
      'A -> Set[Word]("a", "b", "d"),
      'B -> Set[Word]("b", "d", ε),
      'D -> Set[Word]("d", ε))
    unpack(Loner.starters(example2)) shouldEqual Map(
      'S -> Set[Word]("a", "b", "c"),
      'A -> Set[Word]("a", ε),
      'B -> Set[Word]("b", ε))
    unpack(Loner.starters(example3)) shouldEqual Map(
      'S -> Set[Word]("a", "b", "s"),
      'A -> Set[Word]("a", "b", ε),
      'B -> Set[Word]("a", "b"))
    unpack(Loner.starters(example4)) shouldEqual Map(
      'S -> Set[Word]("a", "b"),
      'A -> Set[Word]("a", "b"))
    unpack(Loner.starters(example5)) shouldEqual Map(
      'S -> Set[Word]("a", "b", "d", "f"),
      'A -> Set[Word]("a", "b", "d", "f"),
      'B -> Set[Word]("b", "d", "f"),
      'D -> Set[Word]("d", "f"))
  }

  it should "compute follower sets" in {
    unpack(Loner.followers(example1)) shouldEqual Map(
      'S -> Set.empty,
      'A -> Set[Word]("$"),
      'B -> Set[Word]("a", "b", "d"),
      'D -> Set[Word]("a", "b", "d"))
    unpack(Loner.followers(example2)) shouldEqual Map(
      'S -> Set.empty,
      'A -> Set[Word]("b", "c"),
      'B -> Set[Word]("c"))
    unpack(Loner.followers(example3)) shouldEqual Map(
      'S -> Set.empty,
      'A -> Set[Word]("c", "s"),
      'B -> Set[Word]("c"))
    unpack(Loner.followers(example4)) shouldEqual Map(
      'S -> Set.empty,
      'A -> Set[Word]("a", "b", "c"))
    unpack(Loner.followers(example5)) shouldEqual Map(
      'S -> Set.empty,
      'A -> Set[Word]("x", "$"),
      'B -> Set[Word]("x", "$"),
      'D -> Set[Word]("x", "$"))
  }

  it should "compute the LL(1) property" in {
    Loner.isLLone(example1) shouldEqual false
    Loner.isLLone(example2) shouldEqual false
    Loner.isLLone(example3) shouldEqual true
    Loner.isLLone(example4) shouldEqual false
    Loner.isLLone(example5) shouldEqual true
  }
}
