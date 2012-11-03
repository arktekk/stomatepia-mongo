package stomatepia

object Bson {
  implicit def pairIsDocument(p:(String, Bson)) = BDocument(Seq(p))
}
sealed trait Bson
case class BInt(value:Int) extends Bson{
  override def toString = value.toString
}
case class BString(value:String) extends Bson {
  override def toString = "'" + value + "'"
}
case class BArray(value:Seq[Bson]) extends Bson {
  override def toString = value.mkString("[ ", ", ", " ]")
}
case class BDocument(value:Seq[(String, Bson)]) extends Bson {
  override def toString = value.map{ case (k, v) => "\""+k+"\"" + " : " + v }.mkString("{ ", ", ", " }")
  def ++ (other:BDocument) = BDocument(value ++ other.value) // TODO merge semantics
}
case class BBoolean(value:Boolean) extends Bson {
  override def toString = value.toString
}

trait FromBson[A]{
  def fromBson(bson:Bson):A
}

trait ToBson[A]{
  def toBson(value:A):Bson
}

object ToBson {
  def apply[A : ToBson](value:A) = implicitly[ToBson[A]].toBson(value)

  implicit val boolean = new ToBson[Boolean]{
    def toBson(value: Boolean) = BBoolean(value)
  }

  implicit val int = new ToBson[Int] {
    def toBson(value: Int) = BInt(value)
  }

  implicit val string = new ToBson[String] {
    def toBson(value: String) = BString(value)
  }
}

trait BsonMapper {
  def toDBObject(bson:BDocument):com.mongodb.DBObject
}

object BsonMapper {

  implicit val basic = new BsonMapper{
    import com.mongodb.{BasicDBList, BasicDBObject}

    def toDBObject(bson: BDocument) = {
      val o = new BasicDBObject()
      for ((k,v) <- bson.value){
        o.append(k, toDb(v))
      }
      o
    }

    def toDb(bson:Bson):AnyRef = bson match {
      case doc:BDocument   => toDBObject(doc)
      case BString(value)  => value
      case BInt(value)     => value.asInstanceOf[AnyRef]
      case BBoolean(value) => value.asInstanceOf[AnyRef]
      case BArray(value)   =>
        val a = new BasicDBList
        for(v <- value){
          a.add(toDb(v))
        }
        a
    }
  }
}
