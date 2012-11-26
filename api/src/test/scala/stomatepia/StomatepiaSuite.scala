package stomatepia

import org.scalatest.FunSuite
import java.util.Date

trait StomatepiaSuite extends Stomatepia with ToStringBson with FunSuite {

  private val names = collection.mutable.Set.empty[String]

  class StringIs(what:String){
    def is(s:String) = testNamed(s)

    def testNamed(s:String) = {
      def fresh(i:Int):String = {
        val n = s+" // "+i
        if(names(n))
          fresh(i+1)
        else n
      }
      val name = if(names(s)) fresh(1) else s
      names += name
      test(name){
        assert(what === s)
      }
    }

    def isd(s:String){
      println(s)
      println(what)
      is(s)
    }
  }

  implicit def any2stringIs(any:Any) = new StringIs(any.toString)
}

trait ToStringBson extends StomatepiaBson {

  sealed trait Bson

  object Bson extends BsonProvider {
    def array(elements: Seq[Bson]) = BsonArray(elements)

    def document(fields: Seq[(String, Bson)]) = BsonDocument(fields)

    def int(value: Int) = BInt(value)

    def double(value: Double) = BDouble(value)

    def string(value: String) = BsonString(value)

    def boolean(value: Boolean) = BBoolean(value)

    def regex(value: String, options: String) = BRegex(value, options)

    def binary(data: Array[Byte]) = sys.error("binary")

    def objectId(id: String) = sys.error("objectId")

    def date(value: Date) = sys.error("date")

    def NULL = sys.error("null")

    def javascript(value: String) = sys.error("javascript")

    def jsWithScope(value: String, scope: BsonDocument) = sys.error("jsWithScope")

    def timestamp(time: Int, inc: Int) = sys.error("timestamp")

    def symbol(value: Symbol) = sys.error("symbol")

    def long(value: Long) = sys.error("long")

    def minKey = sys.error("minKey")

    def maxKey = sys.error("maxKey")
  }

  case class BInt(value:Int) extends Bson {
    override def toString = value.toString
  }
  case class BsonString(value:String) extends Bson {
    override def toString = "'" + value + "'"
  }
  case class BsonArray(value:Seq[Bson]) extends Bson {
    override def toString = value.mkString("[ ", ", ", " ]")
  }
  case class BsonDocument(value:Seq[(String, Bson)]) extends Bson {
    override def toString = if(value.isEmpty) "{}" else value.map{ case (k, v) => "\""+k+"\"" + " : " + v }.mkString("{ ", ", ", " }")
  }
  case class BRegex(regex:String, options:String) extends Bson {
    override def toString = "/"+regex+"/" + options
  }
  case class BBoolean(value:Boolean) extends Bson {
    override def toString = value.toString
  }
  case class BDouble(value:Double) extends Bson {
    override def toString = value.toString
  }
}
