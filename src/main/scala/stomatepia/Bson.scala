package stomatepia

sealed trait Bson

case class BInt(value:Int) extends Bson {
  override def toString = value.toString
}
case class BString(value:String) extends Bson {
  override def toString = "'" + value + "'"
}
case class BArray(value:Seq[Bson]) extends Bson {
  override def toString = value.mkString("[ ", ", ", " ]")
}
case class BDocument(value:Seq[(String, Bson)]) extends Bson {
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

  implicit val double = new ToBson[Double] {
    def toBson(value: Double) = BDouble(value)
  }

  implicit val string = new ToBson[String] {
    def toBson(value: String) = BString(value)
  }

  implicit def seq[A : ToBson] = new ToBson[Seq[A]] {
    def toBson(value: Seq[A]) = BArray(value.map(ToBson(_)))
  }

  implicit def tuple2[A : ToBson, B : ToBson] = new ToBson[(A, B)] {
    def toBson(value: (A, B)) = BArray(Seq(ToBson(value._1), ToBson(value._2)))
  }

  implicit def tuple4[A : ToBson, B : ToBson, C : ToBson, D : ToBson] = new ToBson[(A, B, C, D)] {
    def toBson(value: (A, B, C, D)) = BArray(Seq(ToBson(value._1), ToBson(value._2), ToBson(value._3), ToBson(value._4)))
  }
}
