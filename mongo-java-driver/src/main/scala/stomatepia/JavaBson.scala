package stomatepia

import com.mongodb.{BasicDBObject, BasicDBList}
import org.bson.BSON
import java.util.regex.Pattern
import java.util.Date
import org.bson.types._

trait JavaBson extends StomatepiaBson {

  type Bson         = Any
  type BsonDocument = BasicDBObject
  type BsonArray    = BasicDBList
  type BsonString   = String

  object Bson extends BsonProvider {
    def array(elements: Seq[Bson]) = {
      val list = new BasicDBList()
      elements.foreach(b => list.add(b.asInstanceOf[AnyRef]))
      list
    }

    def document(fields: Seq[(String, Bson)]) = {
      val doc = new BasicDBObject()
      fields.foreach{ case (k,v) => doc.put(k, v) }
      doc
    }

    def int(value: Int) = value

    def double(value: Double) = value

    def string(value: String) = value

    def boolean(value: Boolean) = value

    def regex(value: String, options: String) = {
      val flags = BSON.regexFlags(options)
      Pattern.compile(value, flags)
    }

    def binary(data: Array[Byte]) = data

    def objectId(id: String) = new ObjectId(id)

    def date(value: Date) = value

    def NULL = null

    def javascript(value: String) = new Code(value)

    def symbol(value: scala.Symbol) = new Symbol(value.toString)

    def jsWithScope(value: String, scope: BsonDocument) = new CodeWScope(value, scope)

    def timestamp(time:Int, inc:Int) = new BSONTimestamp(time, inc)

    def long(value: Long) = value

    def minKey = new MinKey

    def maxKey = new MaxKey
  }
}
