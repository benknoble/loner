package org.benknoble.ebnf

object Util {
  def join[A](sep: String, list: List[A]): String = list match {
    case h :: t => h.toString() + t.foldLeft("")(_.toString() + sep + _.toString())
    case Nil => ""
  }
}
