package atomic
/**
  * Created by moroz on 24.07.17.
  */
object Atom1 extends App {


  def testBodyMassIndex() = {
    for (i <- 45 to 85) {
      print("weight:" + i + " ")
      bodyMassIndex(i, 1.83f)
    }
  }

  def bodyMassIndex(weight: Float, height: Float): Float = {
    val ret = weight / (height * height)

    if (ret < 18.5) {
      println("Underweight")
    } else if (ret < 25) {
      println("Normal weight")
    } else {
      println("Overweight")
    }
    ret
  }

  def idealWeight(height: Float) = 22 * height * height


  def testDegress() {
    for (i <- 0 to 450) println("Fahrenheit:" + i, "Celsius:" + degreesOfFahrenheitToCelsius(i),
      "Fahrenheit calc:" + degreesOfCelsiusToFahrenheit(degreesOfFahrenheitToCelsius(i)))
  }

  def degreesOfFahrenheitToCelsius(degrees: Float) = ((degrees - 32) * (5 / 9f))

  def degreesOfCelsiusToFahrenheit(degrees: Float) = (degrees * (9 / 5f) + 32)

  def showActivity(activity: String, distance: Float) {

    val iWillDoIt = activity match {
      case "running" => distance <= 6
      case "swimming" => distance <= 1
      case "biking" => distance <= 20
    }

    if (iWillDoIt) println("I will do " + activity + " with distance:" + distance)
    else println("Sorry I can not do " + activity + " with distance " + distance)
  }

  //  val isClose=hours
  //  val isOpen = if (hours >= 9 && hours <= 20) true else false
  //  println(isOpen)
  //  var hours = 6
  //  hours = 10
  //  println(isOpen)

  //  testBodyMassIndex()
  // testDegress()

  println("ideal weigth=" + idealWeight(1.83f))


  showActivity("swimming", 1)
  showActivity("biking", 7)
  showActivity("running", 7)

  val s =
    """
      |sdsfdfdf
      |sdsd
      |fddfgdgfd
      |sdsdwedwe
      |ssdcsdcsd
      |wedwe
    """.stripMargin

  print(s)
}
