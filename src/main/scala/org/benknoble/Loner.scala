package org.benknoble.loner

import org.benknoble.ebnf._

object Loner {

  implicit class Xorable(s: Set[Word]) {
    def ⊙(t: Set[Word]) =
      if (s contains ε)
        (s - ε) union t
      else
        s
  }

  def nullable(g: Grammar): Map[Nonterminal, Boolean] = Map()

  def starters(g: Grammar): Map[Nonterminal, Set[Word]] = Map()

  def followers(g: Grammar): Map[Nonterminal, Set[Word]] = Map()

  def isLLone(g: Grammar): Boolean = false

}
