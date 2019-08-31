package catz

import scala.util._

import cats._
import cats.data._
import cats.implicits._


object MonadEx {

  object ex1 {
    case class Vrm(value: String)

    case class Car(vrm: Vrm, price: Long)

    class CarNotFoundException(vrm: Vrm) extends RuntimeException

    case class CarStore(private val cars: Seq[Car]) {

      private def difference(price1: Long, price2: Long) = price1 - price2

      def priceDifference(vrm1: Vrm, vrm2: Vrm): Long = {
        val carOpt1: Option[Car] = cars.find(_.vrm == vrm1)
        if (carOpt1.isDefined) {
          val carOpt2 = cars.find(_.vrm == vrm2)
          if (carOpt2.isDefined) {
            difference(carOpt1.get.price, carOpt2.get.price)
          } else {
            throw new CarNotFoundException(vrm2)
          }
        } else {
          throw new CarNotFoundException(vrm1)
        }
      }

      def priceDifference2(vrm1: Vrm, vrm2: Vrm): Try[Long] = {
        val carOpt1 = cars.find(_.vrm == vrm1)
        if (carOpt1.isDefined) {
          val carOpt2 = cars.find(_.vrm == vrm2)
          if (carOpt2.isDefined) {
            Success(difference(carOpt1.get.price, carOpt2.get.price))
          } else {
            Failure(new CarNotFoundException(vrm2))
          }
        } else {
          Failure(new CarNotFoundException(vrm1))
        }
      }

      def priceDifference3(vrm1: Vrm, vrm2: Vrm): Try[Long] = {
        cars
          .find(_.vrm == vrm1)
          .fold[Try[Long]](Failure(new CarNotFoundException(vrm1))) { car1 =>
            cars
              .find(_.vrm == vrm2)
              .fold[Try[Long]](Failure(new CarNotFoundException(vrm2))) {
                car2 =>
                  Success(difference(car1.price, car2.price))
              }
          }
      }

      def priceDifference4(vrm1: Vrm, vrm2: Vrm): Try[Long] =
        (for {
          car1 <- cars.find(_.vrm == vrm1)
          car2 <- cars.find(_.vrm == vrm2)
        } yield Success(difference(car1.price, car2.price)))
          .getOrElse(Failure(new CarNotFoundException(vrm1)))

      def priceDifference5(vrm1: Vrm,
                           vrm2: Vrm): Either[CarNotFoundException, Long] =
        for {
          car1 <- cars
            .find(_.vrm == vrm1)
            .toRight(new CarNotFoundException(vrm1))
          car2 <- cars
            .find(_.vrm == vrm2)
            .toRight(new CarNotFoundException(vrm2))
        } yield difference(car1.price, car2.price)

      trait BusinessError

      case class CarNotFound(vrm: Vrm) extends BusinessError

//      def priceDifference6(vrm1: Vrm, vrm2: Vrm): ValidatedNel[Error, Long] = {
//        for {
//          carValidated1 <- Validated.fromOption(cars.find(_.vrm == vrm1), NonEmptyList.of(CarNotFound(vrm1)))
//          carValidated2 <- Validated.fromOption(cars.find(_.vrm == vrm2), NonEmptyList.of(CarNotFound(vrm2)))
//        } yield (carValidated1 , carValidated2)
//
//???
//      }

//      def totalPrice(vrms: Vrm*): ValidatedNel[Error, Long] = {
//        val lookup = (vrm:Vrm) => Validated.fromOption(cars.find(_.vrm == vrm).map(_.price), NonEmptyList.of(CarNotFound(vrm)))
//        vrms.toList.traverseU(lookup).map(_.sum)
//      }

    }

  }

}

object app1 extends App {

  import MonadEx.ex1._
  implicit def strToVrm(a: String): Vrm = Vrm(a)

  val store = CarStore(Seq(Car("aaa", 1), Car("bbb", 1), Car("ccc", 2)))

  val r = store.priceDifference("ccc", "aaa")
  println(s"pd:$r")

//  val r2 = store.priceDifference("ccc", "ddd")
//  println(s"pd:$r2")

  val r3 = store.priceDifference2("ccc", "ddd")
  println(s"pd:$r3")

  val r4 = store.priceDifference4("ccc", "ddd")
  println(s"pd:$r4")

  val r5 = store.priceDifference5("ccc", "ddd")
  println(s"pd:$r5")

  val r6 = store.priceDifference5("eee", "kkk")
  println(s"pd:$r5")

}
