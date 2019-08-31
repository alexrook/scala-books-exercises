package catz

object Monoids extends App {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {

    def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
      monoid

    def assocLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
      m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

    def emptyElemLaw[A](v: A)(implicit m: Monoid[A]): Boolean =
      m.combine(v, m.empty) == v && m.combine(m.empty, v) == v

  }

  object ex1 {
    implicit val booleanIsMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    assert(Monoid.assocLaw(true, false, true))
    assert(Monoid.assocLaw(false, true, true))
    assert(Monoid.emptyElemLaw(true))
    assert(Monoid.emptyElemLaw(false))
  }

  object ex2 {
    implicit val booleanIsMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

    assert(Monoid.assocLaw(true, false, true))
    assert(Monoid.assocLaw(false, true, true))
    assert(Monoid.emptyElemLaw(true))
    assert(Monoid.emptyElemLaw(false))
  }

  object ex3 {
    implicit val booleanIsMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = x == y
    }

    assert(Monoid.assocLaw(true, false, true))
    assert(Monoid.assocLaw(false, true, true))
    assert(Monoid.assocLaw(true, true, true))
    assert(Monoid.assocLaw(false, false, true))
    assert(Monoid.assocLaw(true, true, false))
    assert(Monoid.assocLaw(false, false, false))
    assert(Monoid.emptyElemLaw(true))
    assert(Monoid.emptyElemLaw(false))
  }

  object ex4 {
    implicit val booleanIsMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x ^ y
    }

    assert(Monoid.assocLaw(true, false, true))
    assert(Monoid.assocLaw(false, true, true))
    assert(Monoid.assocLaw(true, true, true))
    assert(Monoid.assocLaw(false, false, true))
    assert(Monoid.assocLaw(true, true, false))
    assert(Monoid.assocLaw(false, false, false))
    assert(Monoid.emptyElemLaw(true))
    assert(Monoid.emptyElemLaw(false))
  }

  ex1
  ex2
  ex3
  ex4
}
