package stomatepia

object Fields {
  def path(t:(Vector[String], Bson)) =
    (t._1.mkString("."), t._2)

  def path(from:Vector[String], t:(Vector[String], Bson)):(String, Bson) =
    path((t._1.drop(from.size), t._2))

  def asArray[X](x:X) =
    x.asInstanceOf[X with IsArray]
}

object Ops {
  implicit def unwrap[A](ops:Ops[A, _]):A = ops.a
}

class Ops[X, A : ToBson](val a:X, route:Vector[String]){
  private def operator[O : ToBson](op:String, value:O) = (route :+ op, ToBson(value))

  def === (value:A)                                     = (route, ToBson(value))
  def < (value:A)                                       = $lt(value)
  def $lt(value:A)                                      = operator("$lt", value)
  def <= (value:A)                                      = $lte(value)
  def $lte(value:A)                                     = operator("$lte", value)
  def > (value:A)                                       = $gt(value)
  def $gt(value:A)                                      = operator("$gt", value)
  def >= (value:A)                                      = $gte(value)
  def $gte(value:A)                                     = operator("$gte", value)
  def $all(values:Seq[A])(implicit ev:X <:< IsArray)    = operator("$all", values)
  def $exists(exists:Boolean)                           = operator("$exists", exists)
  def $mod(mod:Int, rest:Int)                           = operator("$mod", Seq(mod, rest))
  def $ne(value:A)                                      = operator("$ne", value)
  def $in(values:Seq[A])                                = operator("$in", values)
  def $nin(values:Seq[A])                               = operator("$nin", values)
  def $size(size:Int)(implicit ev:X <:< IsArray)        = operator("$size", size)
  def $type(tpe:Int)                                    = operator("$type", tpe)
  def $not(sub:(this.type => (Vector[String], Bson))*)  = operator("$not", BDocument(sub.map(f => Fields.path(route, f(this)))))
  def $regex(regex:String)(implicit ev:A =:= String)    = operator("$regex", regex.replaceAll("\\\\", "\\\\\\\\"))
  def $options(options:String)(implicit ev:A =:= String)= operator("$options", options)

  def $elemMatch(sub:(this.type => (Vector[String], Bson))*)(implicit ev:X <:< IsArray) =
    (route :+ "$elemMatch", BDocument(sub.map(f => Fields.path(route, f(this)))))
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
    private val route               = parent :+ name
    private val ops                 = new Ops[this.type, A](this, route)

    def apply(value:A)                                         = (route, ToBson(value))
    def apply(value:Seq[A])(implicit ev:this.type <:< IsArray) = (route, BArray(value.map(ToBson(_))))
    def apply(sub:(Ops[this.type, A] => (Vector[String], Bson))*) =
      (parent :+ name, BDocument(sub.map(f => Fields.path(parent :+ name, f(ops)))))
  }

  abstract class Field[A <: Field[A]](val parent:Vector[String], name:String) {
    protected def next(name:String):A
    def $(implicit ev:this.type <:< IsArray) = next(name + ".$")
    // TODO conflict with field[Int].apply(Int) ?
    def apply(index:Int)(implicit ev:this.type <:< IsArray) = next(name + "." + index)

    def $elemMatch(sub:(this.type => (Vector[String], Bson))*)(implicit ev:this.type <:< IsArray) =
      (parent :+ name :+ "$elemMatch", BDocument(sub.map(f => Fields.path(parent :+ name, f(this)))))
  }

  abstract class Embedded[X <: Embedded[X]](name:String) extends Field[X](route, name) with Fields {
    embedded: { def copy(name:String):X } =>
    protected def next(name:String) = copy(name)

    def route = self.route :+ name

    def apply(sub:(this.type => (Vector[String], Bson))*) =
      (parent :+ name, BDocument(sub.map(f => Fields.path(parent :+ name, f(this)))))
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




