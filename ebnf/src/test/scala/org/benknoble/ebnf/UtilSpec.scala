package org.benknoble.loner.ebnf

import org.scalatest._

class UtilSpec extends FlatSpec with Matchers {
  "Join on an empty list" should "be empty" in {
    Util.join("a", List()) shouldEqual ""
  }

  "Join on a single item" should "be that item" in {
    Util.join("a", List(1)) shouldEqual "1"
  }

  "Join on a list" should "be that list with a separator" in {
    Util.join("a", List(1,2,3)) shouldEqual "1a2a3"
  }
}
