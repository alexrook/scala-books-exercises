package essentilas

object Section31 extends App {

  object E311 {

    class Cat(name: String, color: String, val food: String) {
      override def toString: String = s"$name, $color color cat eats $food"
    }

    val Oswald = new Cat("Oswald", "Black", "Milk")

    val Henderson = new Cat("Henderson", "Ginger", "Chips")

    val Quentin = new Cat("Quentin", "Tabby and white", "Curry")

    println(Oswald.toString)

  }

  object E312 {

    import E311._

    object ChipShop {
      def willServe(cat: Cat): Boolean = if (cat.food == "Chips") true else false
    }

    println(ChipShop.willServe(Quentin))
    println(ChipShop.willServe(Henderson))
  }

  object E313 {

    /**
      * Director should contain:
        a field firstName of type String
        a field lastName of type String
        a field yearOfBirth of type Int
        a method called name that accepts no parameters and returns the full name
      Film should contain:
          a field name of type String
          a field yearOfRelease of type Int
          a field imdbRating of type Double
          a field director of type Director
          a method directorsAge that returns the age of the director at the me of release
          a method isDirectedBy that accepts a Director as a parameter and returns a Boolean
      */

    class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
      def name = s"$firstName $lastName"
    }

    class Film(val name: String, val yearOfRelease: Int, imdbRating: Double, val director: Director) {
      def directorsAge = yearOfRelease - director.yearOfBirth
      def isDirectedBy(director: Director): Boolean = this.director == director

      def copy(name:          String   = this.name,
               yearOfRelease: Int      = this.yearOfRelease,
               imdbRating:    Double   = this.imdbRating,
               director:      Director = this.director): Film = new Film(name, yearOfRelease, imdbRating, director)

      override def toString: String = s"$name, $yearOfRelease, $imdbRating, ${director.name}"
    }

    val eastwood          = new Director("Clint", "Eastwood", 1930)
    val mcTiernan         = new Director("John", "McTiernan", 1951)
    val nolan             = new Director("Christopher", "Nolan", 1970)
    val someBody          = new Director("Just", "Some Body", 1990)
    val memento           = new Film("Memento", 2000, 8.5, nolan)
    val darkKnight        = new Film("Dark Knight", 2008, 9.0, nolan)
    val inception         = new Film("Inception", 2010, 8.8, nolan)
    val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
    val outlawJoseyWales  = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
    val unforgiven        = new Film("Unforgiven", 1992, 8.3, eastwood)
    val granTorino        = new Film("Gran Torino", 2008, 8.2, eastwood)
    val invictus          = new Film("Invictus", 2009, 7.4, eastwood)
    val predator          = new Film("Predator", 1987, 7.9, mcTiernan)
    val dieHard           = new Film("Die Hard", 1988, 8.3, mcTiernan)
    val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
    val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)
    println(eastwood.yearOfBirth) // should be 1930
    println(dieHard.director.name) // should be "John McTiernan"
    println(invictus.isDirectedBy(nolan)) // should be false

    println(highPlainsDrifter.copy(name = "L'homme des hautes plaines"))
    // returns Film("L'homme des hautes plaines", 1973, 7.7, /* etc */)
    println(thomasCrownAffair.copy(yearOfRelease = 1968, director = new Director("Norman", "Jewison", 1926)))
    // returns Film("The Thomas Crown Affair", 1926, /* etc */)
    println(inception.copy().copy().copy())
    // returns a new copy of `inception`
  }

  object E314 {

    class Adder(amount: Int) {
      def add(in: Int) = in + amount
    }

    class Counter(val count: Int) {
      def inc: Counter = inc()
      def dec: Counter = dec()
      def inc(p: Int = 1): Counter = new Counter(count + p)
      def dec(p: Int = 1): Counter = new Counter(count - p)

      def adjust(adder: Adder) = new Counter(adder.add(count))
    }

    println(new Counter(10).inc.dec.inc.inc.count)
    println(new Counter(10).adjust(new Adder(12)).count)

  }

  E311

  E312

  E313
  E314

}
