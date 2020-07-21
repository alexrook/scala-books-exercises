package shapelezz

import java.util.Calendar

import shapeless.{Generic, Lazy}

object SQLTyped extends App {

  import java.sql.PreparedStatement

  import shapeless.{::, HList, HNil}

  trait OptionalField[T] {
    def mayBeValue: Option[T]

    def javaSqlType: Int
  }

  object OptionalField {

    implicit class optionIsOptionalField[T](v: Option[T]) {

      def withSqlType(sqlType: Int): OptionalField[T] = new OptionalField[T] {
        override def mayBeValue: Option[T] = v

        override def javaSqlType: Int = sqlType
      }
    }

  }

  trait SQLType[-T] {
    def set(stmt: PreparedStatement, index: Int, value: T): Int
  }

  object SQLType {

    def apply[T](implicit ev: SQLType[T]): SQLType[T] = ev

    def instance[T](f: (PreparedStatement, Int, T) => Int): SQLType[T] = new SQLType[T] {
      override def set(stmt: PreparedStatement, index: Int, value: T): Int = f(stmt, index, value)
    }

    implicit val longIsSQLType: SQLType[Long] = instance {
      case (stmt: PreparedStatement, index: Int, value: Long) => {
        stmt.setLong(index, value)
        index
      }
    }

    implicit val stringIsSQLType: SQLType[String] = instance {
      case (stmt: PreparedStatement, index: Int, value: String) => {
        stmt.setString(index, value)
        index
      }
    }

    implicit def OptionalFieldIsSQLType[T](implicit ev: SQLType[T]): SQLType[OptionalField[T]] = instance {
      case (stmt: PreparedStatement, index: Int, value: OptionalField[T]) => {
        value.mayBeValue match {
          case Some(v) => ev.set(stmt, index, v)
          case None => {
            stmt.setNull(index, value.javaSqlType)
            index
          }
        }

      }
    }

    implicit def genericIsSQLType[T, R](implicit gen: Generic.Aux[T, R],
                                        reprSQLType: Lazy[SQLType[R]]): SQLType[T] = instance {
      case (stmt: PreparedStatement, index: Int, value: T) => reprSQLType.value.set(stmt, index, gen.to(value))
    } //TODO: fix or improve : the Type Astronaut’s: 31

    implicit val hNilIsSQLType: SQLType[HNil] = instance {
      case (_: PreparedStatement, index: Int, _: HNil) => index
    }

    implicit def hListIsSQLType[H, T <: HList](
                                                /*без Lazy неработает see Type Astronaut’s: 37*/
                                                implicit extraHead: Lazy[SQLType[H]],
                                                extraTail: SQLType[T]
                                              ): SQLType[H :: T] =
      instance {
        case (stmt: PreparedStatement, index: Int, value: ::[H, T]) =>
          value match {
            case head :: tail => {
              val nextIndex: Int = extraHead.value.set(stmt, index, head)
              extraTail.set(stmt, nextIndex + 1, tail)
              nextIndex + 1

            }
          }
      }

  }

  import OptionalField._

  case class Holder(i: Int, s: String)

  object Holder {

    import SQLType._

    implicit val holderIsSQLType: SQLType[Holder] = instance {
      case (stmt: PreparedStatement, index: Int, value: Holder) => {
        stmt.setInt(index, value.i)
        stmt.setString(index + 1, value.s)
        index + 1
      }
    }
  }

  def execute[T <: HList](args: T)(implicit ev: SQLType[T]): Unit = {
    println("---")
    ev.set(Stub.stmt, 1, args)
    Stub.stmt.execute()
    Stub.clear()
  }

  execute(12L :: "12-HelloWorld" :: HNil)

  execute(12L :: "12-HelloWorld-Some(123L)" :: Some(123L).withSqlType(java.sql.Types.BIGINT) :: HNil)

  execute(Holder(1, "holder") :: 777L :: HNil)

  execute(
    12L :: "12-HelloWorld-Some(Holder(1,'holder'))-777" :: Some(Holder(1, "holder")).withSqlType(java.sql.Types.JAVA_OBJECT) :: 777L :: HNil
  )

  println("---")
  val noneChar: Option[Long] = None

  execute(12L :: "HelloWorld-NoneChar" :: noneChar.withSqlType(java.sql.Types.CHAR) :: HNil)

  case class Holder2(i: Long, sss: String)

  implicit val gen = Generic[Holder2]

  ///import scala.reflect.

  val st = SQLType[Holder]

  println(st)

  val st2 = SQLType[Holder2]

  println(st2)
  //  Holder2(33L,"aaa")
  //

  val h2 = Holder2(23L, "aaa")

  //SQLType.genericIsSQLType[Holder2, Long :: String :: HNil](gen, SQLType[Long :: String :: HNil]).set(Stub.stmt, 1, h2)
  //  Stub.stmt.execute
  execute(Holder2(11, "holder2-11") :: 711L :: HNil)

  execute(
    121L :: "12-HelloWorld-Some(Holder2(1,'holder2'))-7771" :: Holder2(1, "holder2") :: 7771L :: HNil
  )

}


//заглушка для PreparedStatement
object Stub {

  val params = collection.mutable.Map.empty[Int, String]

  def clear() = params.clear()

  import java.io.{InputStream, Reader}
  import java.net.URL
  import java.sql
  import java.sql.{Blob, Clob, Connection, Date, NClob, ParameterMetaData, PreparedStatement, Ref, ResultSet, ResultSetMetaData, RowId, SQLWarning, SQLXML, Time, Timestamp}

  val stmt = new PreparedStatement {

    override def executeQuery(): ResultSet = ???

    override def executeUpdate(): Int = ???

    override def setNull(parameterIndex: Int, sqlType: Int): Unit = params(parameterIndex) = s"Null, with sqlType:$sqlType"

    override def setBoolean(parameterIndex: Int, x: Boolean): Unit = ???

    override def setByte(parameterIndex: Int, x: Byte): Unit = params(parameterIndex) = s"$x: Byte"

    override def setShort(parameterIndex: Int, x: Short): Unit = ???

    override def setInt(parameterIndex: Int, x: Int): Unit = params(parameterIndex) = s"$x: Int"

    override def setLong(parameterIndex: Int, x: Long): Unit = params(parameterIndex) = s"$x: Long"

    override def setFloat(parameterIndex: Int, x: Float): Unit = ???

    override def setDouble(parameterIndex: Int, x: Double): Unit = ???

    override def setBigDecimal(parameterIndex: Int, x: java.math.BigDecimal): Unit = ???

    override def setString(parameterIndex: Int, x: String): Unit = params(parameterIndex) = s"$x: String"

    override def setBytes(parameterIndex: Int, x: Array[Byte]): Unit = ???

    override def setDate(parameterIndex: Int, x: Date): Unit = ???

    override def setTime(parameterIndex: Int, x: Time): Unit = ???

    override def setTimestamp(parameterIndex: Int, x: Timestamp): Unit = ???

    override def setAsciiStream(parameterIndex: Int, x: InputStream, length: Int): Unit = ???

    override def setUnicodeStream(parameterIndex: Int, x: InputStream, length: Int): Unit = ???

    override def setBinaryStream(parameterIndex: Int, x: InputStream, length: Int): Unit = ???

    override def clearParameters(): Unit = ???

    override def setObject(parameterIndex: Int, x: scala.Any, targetSqlType: Int): Unit = ???

    override def setObject(parameterIndex: Int, x: scala.Any): Unit = ???

    override def execute(): Boolean = {
      params.toSeq.sortBy(_._1).foreach {
        case (k, v) => println(s"param:index[$k], value[$v]")
      }
      true
    }

    override def addBatch(): Unit = ???

    override def setCharacterStream(parameterIndex: Int, reader: Reader, length: Int): Unit = ???

    override def setRef(parameterIndex: Int, x: Ref): Unit = ???

    override def setBlob(parameterIndex: Int, x: Blob): Unit = ???

    override def setClob(parameterIndex: Int, x: Clob): Unit = ???

    override def setArray(parameterIndex: Int, x: sql.Array): Unit = ???

    override def getMetaData: ResultSetMetaData = ???

    override def setDate(parameterIndex: Int, x: Date, cal: Calendar): Unit = ???

    override def setTime(parameterIndex: Int, x: Time, cal: Calendar): Unit = ???

    override def setTimestamp(parameterIndex: Int, x: Timestamp, cal: Calendar): Unit = ???

    override def setNull(parameterIndex: Int, sqlType: Int, typeName: String): Unit = ???

    override def setURL(parameterIndex: Int, x: URL): Unit = ???

    override def getParameterMetaData: ParameterMetaData = ???

    override def setRowId(parameterIndex: Int, x: RowId): Unit = ???

    override def setNString(parameterIndex: Int, value: String): Unit = ???

    override def setNCharacterStream(parameterIndex: Int, value: Reader, length: Long): Unit = ???

    override def setNClob(parameterIndex: Int, value: NClob): Unit = ???

    override def setClob(parameterIndex: Int, reader: Reader, length: Long): Unit = ???

    override def setBlob(parameterIndex: Int, inputStream: InputStream, length: Long): Unit = ???

    override def setNClob(parameterIndex: Int, reader: Reader, length: Long): Unit = ???

    override def setSQLXML(parameterIndex: Int, xmlObject: SQLXML): Unit = ???

    override def setObject(parameterIndex: Int, x: scala.Any, targetSqlType: Int, scaleOrLength: Int): Unit = ???

    override def setAsciiStream(parameterIndex: Int, x: InputStream, length: Long): Unit = ???

    override def setBinaryStream(parameterIndex: Int, x: InputStream, length: Long): Unit = ???

    override def setCharacterStream(parameterIndex: Int, reader: Reader, length: Long): Unit = ???

    override def setAsciiStream(parameterIndex: Int, x: InputStream): Unit = ???

    override def setBinaryStream(parameterIndex: Int, x: InputStream): Unit = ???

    override def setCharacterStream(parameterIndex: Int, reader: Reader): Unit = ???

    override def setNCharacterStream(parameterIndex: Int, value: Reader): Unit = ???

    override def setClob(parameterIndex: Int, reader: Reader): Unit = ???

    override def setBlob(parameterIndex: Int, inputStream: InputStream): Unit = ???

    override def setNClob(parameterIndex: Int, reader: Reader): Unit = ???

    override def executeQuery(sql: String): ResultSet = ???

    override def executeUpdate(sql: String): Int = ???

    override def close(): Unit = ???

    override def getMaxFieldSize: Int = ???

    override def setMaxFieldSize(max: Int): Unit = ???

    override def getMaxRows: Int = ???

    override def setMaxRows(max: Int): Unit = ???

    override def setEscapeProcessing(enable: Boolean): Unit = ???

    override def getQueryTimeout: Int = ???

    override def setQueryTimeout(seconds: Int): Unit = ???

    override def cancel(): Unit = ???

    override def getWarnings: SQLWarning = ???

    override def clearWarnings(): Unit = ???

    override def setCursorName(name: String): Unit = ???

    override def execute(sql: String): Boolean = ???

    override def getResultSet: ResultSet = ???

    override def getUpdateCount: Int = ???

    override def getMoreResults: Boolean = ???

    override def setFetchDirection(direction: Int): Unit = ???

    override def getFetchDirection: Int = ???

    override def setFetchSize(rows: Int): Unit = ???

    override def getFetchSize: Int = ???

    override def getResultSetConcurrency: Int = ???

    override def getResultSetType: Int = ???

    override def addBatch(sql: String): Unit = ???

    override def clearBatch(): Unit = ???

    override def executeBatch(): Array[Int] = ???

    override def getConnection: Connection = ???

    override def getMoreResults(current: Int): Boolean = ???

    override def getGeneratedKeys: ResultSet = ???

    override def executeUpdate(sql: String, autoGeneratedKeys: Int): Int = ???

    override def executeUpdate(sql: String, columnIndexes: Array[Int]): Int = ???

    override def executeUpdate(sql: String, columnNames: Array[String]): Int = ???

    override def execute(sql: String, autoGeneratedKeys: Int): Boolean = ???

    override def execute(sql: String, columnIndexes: Array[Int]): Boolean = ???

    override def execute(sql: String, columnNames: Array[String]): Boolean = ???

    override def getResultSetHoldability: Int = ???

    override def isClosed: Boolean = ???

    override def setPoolable(poolable: Boolean): Unit = ???

    override def isPoolable: Boolean = ???

    override def closeOnCompletion(): Unit = ???

    override def isCloseOnCompletion: Boolean = ???

    override def unwrap[T](iface: Class[T]): T = ???

    override def isWrapperFor(iface: Class[_]): Boolean = ???
  }

}
