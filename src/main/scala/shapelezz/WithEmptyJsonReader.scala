package shapelezz

import shapeless.labelled.FieldType
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}
import spray.json.{JsNull, JsValue, _}

trait WithEmptyJsonReader[T] {

  def read(json: JsValue): T

}

object WithEmptyJsonReader {

  def apply[T](implicit ev: WithEmptyJsonReader[T]): WithEmptyJsonReader[T] = ev

  implicit def optionReader[T](implicit ev: WithEmptyJsonReader[T]): WithEmptyJsonReader[Option[T]] =
    new WithEmptyJsonReader[Option[T]] {
      override def read(json: JsValue): Option[T] = json match {
        case JsNull => None
        case other => Option(ev.read(other))
      }
    }

  implicit val booleanReader: WithEmptyJsonReader[Boolean] =
    new WithEmptyJsonReader[Boolean] {
      override def read(json: JsValue): Boolean = json match {
        case JsBoolean(value) => value
        case _ => throw new Exception(s"unsupported json type[$json]")
      }
    }

  implicit val intReader: WithEmptyJsonReader[Int] =
    new WithEmptyJsonReader[Int] {
      override def read(json: JsValue): Int = json match {
        case JsNumber(value) => value.intValue()
        case _ => throw new Exception(s"unsupported json type[$json]")
      }
    }

  implicit val stringReader: WithEmptyJsonReader[String] =
    new WithEmptyJsonReader[String] {
      override def read(json: JsValue): String = json match {
        case JsString(value) => value
        case _ => throw new Exception(s"unsupported json type[$json]")
      }
    }

  implicit def listReader[T](implicit ev: WithEmptyJsonReader[T]): WithEmptyJsonReader[List[T]] =
    new WithEmptyJsonReader[List[T]] {
      override def read(json: JsValue): List[T] = json match {
        case JsArray(value) => value.map(ev.read).toList
        case _ => throw new Exception(s"unsupported json type[$json]")
      }
    }


  implicit val hNilReader: WithEmptyJsonReader[HNil] = new WithEmptyJsonReader[HNil] {
    override def read(json: JsValue): HNil = HNil
  }

  implicit def hlistReader[K <: Symbol, H, T <: HList](
                                                        implicit witness: Witness.Aux[K],
                                                        hasEmpty: HasEmpty[H],
                                                        hEncoder: Lazy[WithEmptyJsonReader[H]],
                                                        tEncoder: WithEmptyJsonReader[T],
                                                      ): WithEmptyJsonReader[FieldType[K, H] :: T] =
    new WithEmptyJsonReader[FieldType[K, H] :: T] {
      override def read(json: JsValue): FieldType[K, H] :: T = {
        val fieldName: String = witness.value.name
        json match {
          case JsObject(fields) =>
            val head: H = fields.get(fieldName).map {
              js => hEncoder.value.read(js)
            }.getOrElse(hasEmpty.empty)

            labelled.field[K](head) :: tEncoder.read(JsObject(fields - fieldName))

          case _ => throw new Exception(s"unsupported json type[$json]")
        }
      }
    }

  implicit def genericReader[T, Repr](
                                       implicit
                                       lbGen: LabelledGeneric.Aux[T, Repr],
                                       encoder: Lazy[WithEmptyJsonReader[Repr]]
                                     ): WithEmptyJsonReader[T] =
    new WithEmptyJsonReader[T] {
      override def read(json: JsValue): T = lbGen.from(encoder.value.read(json))
    }

}


object encApp extends App with DefaultJsonProtocol {

  case class Holder(a: Boolean, b: String, e: List[Boolean])

  case class HighOrderHolder(c: Int, d: Holder)

  implicit val genHolder = Generic[Holder]

  implicit val lbGenHolder = LabelledGeneric[Holder]

  val json1: JsValue =
    """|{
       | "a":true,
       | "b":"bbb"
       |}
       """.stripMargin.parseJson

  val ret = WithEmptyJsonReader[Holder].read(json1)

  println(ret)

  val json2: JsValue =
    """|{
       | "c":3,
       | "d":{
       |        "a":true
       |      }
       |}
         """.stripMargin.parseJson


  val ret2 = WithEmptyJsonReader[HighOrderHolder].read(json2)

  println(ret2)

  val json3: JsValue =
    """|[
       | "a",
       | "b",
       | "c"
       |]
         """.stripMargin.parseJson


  val ret3 = WithEmptyJsonReader[List[String]].read(json3)

  println(ret3)

  //--

  case class OptTest(a: Option[Int])

  implicit val genOptTest = Generic[OptTest]
  implicit val lbgenOptTest = LabelledGeneric[OptTest]

  val json4 =
    """
      |{"a":12}
      |""".stripMargin.parseJson

  val ret4 = WithEmptyJsonReader[OptTest].read(json4)

  println(ret4)


}