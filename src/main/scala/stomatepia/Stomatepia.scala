package stomatepia

object Fields {
  def name(field:Fields#Field[_]) = field.__route.mkString(".")

  def path(t:(Vector[String], Bson)) =
    (t._1.mkString("."), t._2)

  def path(from:Vector[String], t:(Vector[String], Bson)):(String, Bson) =
    path((t._1.drop(from.size), t._2))

  def array[X](x:X) =
    x.asInstanceOf[X with IsArray]

  def bsonDocument[A](from:Vector[String], name:String, what:A, fields:Seq[A => (Vector[String], Bson)]) =
    (from :+ name, BDocument(fields.map(f => path(from, f(what)))))

  def bsonArray[A](from:Vector[String], name:String, what:A, fields:Seq[A => (Vector[String], Bson)]) =
    (from :+ name, BArray(fields.map(f => BDocument(Seq(path(from, f(what)))))))
}

object Ops {
  implicit def unwrap[A](ops:Ops[A, _]):A = ops.underlying
}

class Ops[X, A : ToBson](val underlying:X, route:Vector[String]){
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
  def $not(regex:String, options:String = "")(implicit ev: A =:= String)      = operator("$not", BRegex(regex, options))
  def $regex(regex:String)(implicit ev:A =:= String)    = operator("$regex", regex.replaceAll("\\\\", "\\\\\\\\"))
  def $options(options:String)(implicit ev:A =:= String)= operator("$options", options)

  def $elemMatch(sub:(this.type => (Vector[String], Bson))*)(implicit ev:X <:< IsArray) =
    (route :+ "$elemMatch", BDocument(sub.map(f => Fields.path(route, f(this)))))

  //geo
  def $near(x:Int, y:Int)(implicit ev: A =:= (Int, Int)) = operator("$near", (x, y))
  def $nearSphere(x:Int, y:Int)(implicit ev: A =:= (Int, Int)) = operator("$nearSphere", (x, y))
  def $centerSphere(point:(Int, Int), radius:Int)(implicit ev: A =:= (Int, Int)) = operator("$centerSphere", (point, radius))
  def $maxDistance(maxDistance:Int)(implicit ev: A =:= (Int, Int)) = operator("$maxDistance", maxDistance)
  def $within(within:(Within => (Vector[String], Bson))*)(implicit ev: A =:= (Int, Int)) =
    (route :+ "$within", BDocument(within.map(w => Fields.path(route :+ "$within", w(new Within)))))

  class Within {
    private val within = route :+ "$within"
    def $box(bottomLeft:(Int, Int), topRight:(Int, Int)) = (within :+ "$box", ToBson((bottomLeft, topRight)))
    def $box(a:(Int, Int), b:(Int, Int), c:(Int, Int), d:(Int, Int)) = (within :+ "$box", ToBson((a, b, c, d)))
    def $center(center:(Int, Int), radius:Int) = (within :+ "$center", ToBson((center, radius)))
    def $polygon(points:(Int, Int)*) = (within :+ "$polygon", ToBson(points))
    def $uniqueDocs(uniqueDocs:Boolean) = (within :+ "$uniqueDocs", ToBson(uniqueDocs))
  }

  //update
  def $each(values:Seq[A]) = operator("$each", values)

  //find subset
  def $slice(slice:Int) = operator("$slice", slice)
  def $slice(skip:Int, limit:Int) = operator("$slice", (skip, limit))
}

object ApplyOps {
  implicit def next[A](applyOps:ApplyOps[A]) = applyOps.next
  implicit def toBson[A](a:ApplyOps[A]) = (a.route, BInt(a.value))
}

class ApplyOps[X](val next:X, val route:Vector[String], val value:Int)

trait Fields { self =>

  def __route:Vector[String]

  private type ARG = self.type => (Vector[String], Bson)

  def int(name:String)         = new Primitive[Int](name)
  def double(name:String)      = new Primitive[Double](name)
  def string(name:String)      = new Primitive[String](name)
  def boolean(name:String)     = new Primitive[Boolean](name)
  def geolocation(name:String) = new Primitive[(Int, Int)](name)

  // query
  def $or(or:ARG*) =
    Fields.bsonArray[self.type](__route, "$or", self, or)
  def $nor(nor:ARG*) =
    Fields.bsonArray[self.type](__route, "$nor", self, nor)
  def $and(and:ARG*) =
    Fields.bsonArray[self.type](__route, "$and", self, and)
  def $where(where:String) =
    (__route :+ "$where", BString(where))

  // update
  def $set(set:ARG*) =
    Fields.bsonDocument[self.type](__route, "$set", self, set)
  def $unset(unset:ARG*) =
    Fields.bsonDocument[self.type](__route, "$unset", self, unset)
  def $inc(inc:ARG*) =
    Fields.bsonDocument[self.type](__route, "$inc", self, inc)
  def $rename(rename:(self.type => Fields#Field[_], self.type => Fields#Field[_])*) =
    (__route :+ "$rename", BDocument(rename.map{ case (from, to) => Fields.name(from(this)) -> ToBson(Fields.name(to(this))) }))
  def $push(push:ARG*) =
    Fields.bsonDocument[self.type](__route, "$push", self, push)
  def $pushAll(pushAll:(self.type => (Vector[String], BArray))*) =
    Fields.bsonDocument[self.type](__route, "$pushAll", self, pushAll)
  def $addToSet(addToSet:ARG*) =
    Fields.bsonDocument[self.type](__route, "$addToSet", self, addToSet)
  def $pop(pop:ARG*) =
    Fields.bsonDocument[self.type](__route, "$pop", self, pop)
  def $pull(pull:ARG*) =
    Fields.bsonDocument[self.type](__route, "$pull", self, pull)
  def $pullAll(pullAll:ARG*) =
    Fields.bsonDocument[self.type](__route, "$pullAll", self, pullAll)

  class Primitive[A] (parent:Vector[String], val name:String)(implicit toBson:ToBson[A]) extends Field[Primitive[A]](parent, name){ primitive =>
    def this(name:String)(implicit toBson:ToBson[A]) = this(__route, name)

    protected def next(name:String) = new Primitive[A](name)
    private val ops                 = new Ops[this.type, A](this, __route)

    def apply(value:A)                                                = (__route, ToBson(value))
    def apply(value:Seq[A])(implicit ev:this.type <:< IsArray)        = (__route, BArray(value.map(ToBson(_))))
    def apply(regex:String, options:String)(implicit ev:A =:= String) = (__route, BRegex(regex, options))
    def apply(sub:(Ops[this.type, A] => (Vector[String], Bson))*) =
      (__route, BDocument(sub.map(f => Fields.path(__route, f(ops)))))

    def $(value:A)(implicit ev:this.type <:< IsArray) = super.$.apply(value)
  }

  abstract class Embedded[X <: Embedded[X]](name:String) extends Field[X](__route, name) with Fields {
    embedded: { def copy(name:String):X } =>
    protected def next(name:String) = copy(name)

    def apply(sub:(this.type => (Vector[String], Bson))*) =
      (__route, BDocument(sub.map(f => Fields.path(__route, f(this)))))
  }

  sealed abstract class Field[A <: Field[A]](val parent:Vector[String], name:String) {
    protected def next(name:String):A
    def __route = parent :+ name

    def $(implicit ev:this.type <:< IsArray) = next(name + ".$")
    def apply(index:Int) = new ApplyOps[A](next(name + "." + index), __route, index)

    def $elemMatch(elemMatch:(this.type => (Vector[String], Bson))*)(implicit ev:this.type <:< IsArray) =
      Fields.bsonDocument[this.type](parent :+ name, "$elemMatch", this, elemMatch)

    // TODO - Ops
    def $slice(slice:Int) = (__route :+ "$slice", ToBson(slice))
    def $slice(skip:Int, limit:Int) = (__route :+ "$slice", ToBson((skip, limit)))
  }
}

trait Collections { self: Fields =>
  case class Collection(name:String) {

    def find(fields:(self.type => (Vector[String], Bson))*) =
      Find[self.type](self, name, BDocument(fields.map(f => Fields.path(f(self)))))

    def find(js:String) =
      Find[self.type](self, name, BString(js))

    def findOne(fields:(self.type => (Vector[String], Bson))*) =
      FindOne(name, BDocument(fields.map(f => Fields.path(f(self)))))

    def insert(a:(self.type => (Vector[String], Bson))*){}

    def update(q:(self.type => (Vector[String], Bson))*)(u:(self.type => (Vector[String], Bson))*) =
      Update(name, BDocument(q.map(f => Fields.path(f(self)))), BDocument(u.map(f => Fields.path(f(self)))))

    def remove{}

    def ensureIndex(on:(self.type => (Vector[String], BInt))*){}

    def reIndex(){}

    def save(value:(self.type => Any)*){}
  }
}

trait IsArray

trait Stomatepia extends Fields with Collections {
  def __route = Vector.empty[String]
  def array[X](value:X) = Fields.array(value)
}

trait Finder[A <: Fields] {
  def from:A
  def sort(by:(A => (Vector[String], Bson))*){}
}

case class Find[A <: Fields](from:A, collection:String, query:Bson) extends Finder[A] {
  override def toString = "db."+collection+".find("+ query +")"

  def apply(fields:(A => (Vector[String], Bson))*) = FindSubset[A](collection, query, BDocument(fields.map(f => Fields.path(f(from)))))
}

case class FindSubset[A <: Fields](collection:String, query:Bson, subset:Bson){
  override def toString = "db."+collection+".find("+query+", "+subset+")"
}

case class FindOne(collection:String, query:Bson){
  override def toString = "db."+collection+".findOne("+query+")"
}

case class Update(collection:String, params:Bson*){
  override def toString = "db."+collection+".update("+params.mkString(", ")+")"
}

case class Save(collection:String, document:Bson){
  override def toString = "db."+collection+".save("+document+")"
}




