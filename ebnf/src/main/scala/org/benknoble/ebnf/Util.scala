package org.benknoble.loner.ebnf

object Util {
  def join[A](sep: String, list: Seq[A]): String = list match {
    case h :: t => h.toString() + t.foldLeft("")(_.toString() + sep + _.toString())
    case Nil => ""
  }
}
