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

trait ToBson[A]{
  def toBson(value:A):Bson
}

object ToBson {
  def apply[A : ToBson](value:A) = implicitly[ToBson[A]].toBson(value)

  implicit def identity[A <: Bson] = new ToBson[A] {
    def toBson(value: A) = value
  }

  implicit val boolean = new ToBson[Boolean]{
    def toBson(value: Boolean) = BBoolean(value)
  }

  implicit val int = new ToBson[Int] {
    def toBson(value: Int) = BInt(value)
  }

  implicit val string = new ToBson[String] {
    def toBson(value: String) = BString(value)
  }

  implicit def seq[A : ToBson] = new ToBson[Seq[A]] {
    def toBson(value: Seq[A]) = BArray(value.map(ToBson(_)))
  }
}
