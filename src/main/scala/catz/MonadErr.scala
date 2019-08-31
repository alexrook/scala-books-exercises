package catz

import cats.MonadError
import cats.implicits._
import scala.util.{Failure, Success, Try}

object MonadErr {

  import scala.language.higherKinds

  case class ParseException(m:String) extends Exception

  class JsonParser[F[_]](implicit M: MonadError[F, Throwable]) {

    def toDouble(v:String):Option[Double]=Try(v.toDouble).toOption

    def toJson(str: String): F[Double] =
      toDouble(str) match {
        case None =>M.raiseError(ParseException("Could not parse JSON String"))
        case Some(value) => M.pure(value)
      }

    def getHead(seq:Seq[String]):F[String]=if (seq.nonEmpty) {
      M.pure(seq.head)
    }else {
      M.raiseError(ParseException("seq is empty"))
    }

    def seqHeadToDouble(seq: Seq[String]): F[Double] = {
      for {
        content <- getHead(seq)
        parsed <- toJson(content)
      } yield parsed
    }
  }

}

object app extends App{

  def printr(r:Try[Double]): Unit =
    r match {
      case Success(value)=>println(value)
      case Failure(exception)=>println(exception.getMessage)
    }

  import MonadErr._

  val cls=new JsonParser[Try]

  val r: Try[Double] =cls.seqHeadToDouble(Seq.empty[String])
   printr(r)


}