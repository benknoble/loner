package org.benknoble.loner;

/** A subgroup over members of type A
  *
  * From category theory: A subgroup is a domain of elements (A) and an
  * associative binary operation
  */
trait Subgroup[A] {
  /** The associative binary operation of the subgroup */
  def apply(a: A)(b: A): A
}

/** A moinoid over members of type A
  *
  * From category theory: A monoid is a subgroup equipped with a left-and-right
  * identity value called zero
  */
trait Monoid[A] extends Subgroup[A] {
  /** The identity element */
  def zero: A
}

/** A set-filtering monoid.
  *
  * Given an element `a` of the domain, the identity element is `{a}` and the
  * subgroup operation is `S * T = { S if ¬(a ∈ S); (S - {a}) ∪ T if a ∈ S }`
  *
  * @constructor create a SetFilter over element a
  * @param a the zero element
  */
case class SetFilter[A](a: A) extends Monoid[Set[A]] {
  def zero = Set(a)

  def apply(s: Set[A])(t: Set[A]): Set[A] =
    if (s contains a)
      (s - a) union t
    else
      s
}
