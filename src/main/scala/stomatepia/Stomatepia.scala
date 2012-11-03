package stomatepia

object Fields {
  def drop(i:Int, t:(Vector[String], Bson)) =
    (t._1.drop(i), t._2)
  def path(t:(Vector[String], Bson)) =
    (t._1.mkString("."), t._2)
  def document(t:(Vector[String], Bson)) = t match {
    case (Seq(), doc:BDocument) => doc
    case _ => BDocument(Seq(path(t)))
  }

  def asArray[X](x:X) = x.asInstanceOf[X with IsArray]
}

trait Fields { self =>

  def route:Vector[String]

  def int(name:String)     = new Primitive[Int](name)
  def string(name:String)  = new Primitive[String](name)
  def boolean(name:String) = new Primitive[Boolean](name)

  def $or(or:(self.type => (Vector[String], Bson))*) = (Vector("$or"), BDocument(or.map(f => Fields.path(f(self)))))
  def $and(and:(self.type => (Vector[String], Bson))*) = (Vector("$and"), BArray(and.map(f => BDocument(Seq(Fields.path(f(self)))))))
  def $where(where:String) = (Vector("$where"), BString(where))

  class Primitive[A] private (parent:Vector[String], name:String)(implicit toBson:ToBson[A]) extends Field[Primitive[A]](parent, name){ primitive =>
    def this(name:String)(implicit toBson:ToBson[A]) = this(route, name)

    protected def next(name:String) = new Primitive[A](name)
    private def route = parent :+ name

    private def operator(op:String, value:A) =
      apply(_ => new Primitive[A](route, op).apply(value))

    def apply(value:A)          = (route, ToBson(value))
    def apply(value:Seq[A])(implicit ev:this.type <:< IsArray) = (route, BArray(value.map(ToBson(_))))
    def === (value:A)           = (route, ToBson(value))
    def < (value:A)             = $lt(value)
    def $lt(value:A)            = operator("$lt", value)
    def <= (value:A)            = $lte(value)
    def $lte(value:A)           = (route, BDocument(Seq("$lte" -> ToBson(value))))
    def > (value:A)             = $gt(value)
    def $gt(value:A)            = (route, BDocument(Seq("$gt" -> ToBson(value))))
    def >= (value:A)            = $gte(value)
    def $gte(value:A)           = (route, BDocument(Seq("$gte" -> ToBson(value))))
    def $all(values:Seq[A])(implicit ev:this.type <:< IsArray) = (route, BDocument(Seq("$all" -> BArray(values.map(ToBson(_))))))
    def $exists(exists:Boolean) = (route, BDocument(Seq("$exists" -> BBoolean(exists))))
    def $mod(mod:Int, rest:Int) = (route, BDocument(Seq("$mod" -> BArray(Seq(BInt(mod), BInt(rest))))))
    def $ne(value:A)            = (route, BDocument(Seq("$ne" -> ToBson(value))))
    def $in(values:Seq[A])      = (route, BDocument(Seq("$in" -> BArray(values.map(ToBson(_))))))
    def $nin(values:Seq[A])     = apply(_ => Fields.asArray(new Primitive[A](route, "$nin")).apply(values))//(route, BDocument(Seq("$nin" -> BArray(values.map(ToBson(_))))))
    def $size(size:Int)(implicit ev:this.type <:< IsArray) = (route, BDocument(Seq("$size" -> BInt(size))))
    def $type(tpe:Int)          = (route, BDocument(Seq("$type" -> BInt(tpe))))
    def $not(sub:(this.type => (Vector[String], Bson))*) = (route, BDocument(Seq("$not" -> BDocument(sub.map(f => Fields.path(Fields.drop(route.size, f(this))))))))
    def $regex(regex:String)(implicit ev:A =:= String)    = (Vector(name, "$regex"), BString(regex.replaceAll("\\\\", "\\\\\\\\")))
    def $options(options:String)(implicit ev:A =:= String)= (Vector(name, "$options"), BString(options))
  }

  abstract class Field[A <: Field[A]](parent:Vector[String], name:String) {
    protected def next(name:String):A
    def $(implicit ev:this.type <:< IsArray) = next(name + ".$")
    def apply(index:Int)(implicit ev:this.type <:< IsArray) = next(name + "." + index)
    def apply(sub:(this.type => (Vector[String], Bson))*) = (parent :+ name, sub.foldLeft(BDocument(Seq())){ (a,b) => a ++ Fields.document(Fields.drop(route.size + 1, b(this)))})//.map(f => Fields.document(Fields.drop(route.size + 1, f(this))))))
    def $elemMatch(sub:(this.type => (Vector[String], Bson))*)(implicit ev:this.type <:< IsArray) = (route :+ name, BDocument(Seq("$elemMatch" -> BDocument(sub.map(f => Fields.path(Fields.drop(1, f(this))))))))

  }

  abstract class Embedded[X <: Embedded[X]](name:String) extends Field[X](route, name) with Fields {
    embedded: { def copy(name:String):X } =>
    protected def next(name:String) = copy(name)

    def route = self.route :+ name
  }
}

trait Collections { self: Fields with Base =>
  case class Collection(name:String) {
    def find(fields:(self.type => (Vector[String], Bson))*) = Cursor(name, "find", BDocument(fields.map(f => Fields.path(f(self)))))
    def find(js:String) = Cursor(name, "find", BString(js))
    def findOne(fields:(self.type => (Vector[String], Bson))*) = Cursor(name, "findOne", BDocument(fields.map(f => Fields.path(f(self)))))
    def insert(a:(self.type => (Vector[String], Bson))*){}
    def update{}
    def remove{}
    def ensureIndex(on:self.type => Fields#Field[_]){}
    def reIndex(){}
    def save(value:(self.type => Any)*){}
  }
}

trait Base { self:Fields =>
  val _id = string("_id")
  def array[X](value:X) = Fields.asArray(value)
}

trait IsArray

trait Stomatepia extends Fields with Base with Collections {
  def route = Vector.empty[String]
}

case class Cursor(collection:String, method:String, query:Bson){
//  println(toString)
  override def toString = "db."+collection+"."+method+"("+ query +")"
}




