package org.benknoble.loner;

trait Subgroup[A] {
  // must be associative
  def apply(a: A)(b: A): A
}

trait Monoid[A] extends Subgroup[A] {
  def zero: A
}

case class SetFilter[A](a: A) extends Monoid[Set[A]] {
  def zero = Set(a)

  def apply(s: Set[A])(t: Set[A]): Set[A] =
    if (s contains a)
      (s - a) union t
    else
      s
}
