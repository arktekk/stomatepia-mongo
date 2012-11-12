package stomatepia

/**
 * TODO:
 * - typeclasses for Numeriske (+Numeriske tupled) parametere
 * - bedre oppløsning på Bson typer (Doc, Array, Int osv)
 * - abstrakte typer for innput og output fra bson mapping
 * - javaBson i test som eget prosjekt m/eksekvering av kode
 * - phantomtypes ala rougue for bedre typesjekking av gyldige parameter kombinasjoner
 */
trait Stomatepia extends StomatepiaBson {

  private object Fields {
    def name(field:Fields#Field[_]) = field.__route.mkString(".")

    def path(t:(Vector[String], Bson)) =
      (t._1.mkString("."), t._2)

    def path(from:Vector[String], t:(Vector[String], Bson)):(String, Bson) =
      path((t._1.drop(from.size), t._2))

    def array[X](x:X) =
      x.asInstanceOf[X with IsArray]

    def bsonDocument[A <: Route](name:String, what:A, fields:Seq[A => (Vector[String], Bson)]) =
      (what.__route :+ name, Bson.document(fields.map(f => path(what.__route, f(what)))))

    def bsonArray[A <: Route](name:String, what:A, fields:Seq[A => (Vector[String], Bson)]) =
      (what.__route :+ name, Bson.array(fields.map(f => Bson.document(Seq(path(what.__route, f(what)))))))
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
    def $not(sub:(this.type => (Vector[String], Bson))*)  = operator("$not", Bson.document(sub.map(f => Fields.path(route, f(this)))))
    def $not(regex:String, options:String = "")(implicit ev: A =:= String) = operator("$not", Bson.regex(regex, options))
    def $regex(regex:String)(implicit ev:A =:= String)    = operator("$regex", regex.replaceAll("\\\\", "\\\\\\\\"))
    def $options(options:String)(implicit ev:A =:= String)= operator("$options", options)

    def $elemMatch(sub:(this.type => (Vector[String], Bson))*)(implicit ev:X <:< IsArray) =
      (route :+ "$elemMatch", Bson.document(sub.map(f => Fields.path(route, f(this)))))

    //geo
    def $near(x:Int, y:Int)(implicit ev: A =:= (Int, Int)) = operator("$near", (x, y))
    def $nearSphere(x:Int, y:Int)(implicit ev: A =:= (Int, Int)) = operator("$nearSphere", (x, y))
    def $centerSphere(point:(Int, Int), radius:Int)(implicit ev: A =:= (Int, Int)) = operator("$centerSphere", (point, radius))
    def $maxDistance(maxDistance:Int)(implicit ev: A =:= (Int, Int)) = operator("$maxDistance", maxDistance)
    def $within(within:(Within => (Vector[String], Bson))*)(implicit ev: A =:= (Int, Int)) =
      (route :+ "$within", Bson.document(within.map(w => Fields.path(route :+ "$within", w(new Within)))))

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
    implicit def next[A](a:ApplyOps[A]) = a.next
    implicit def toBson[A](a:ApplyOps[A]) = (a.route, ToBson(a.value))
  }

  class ApplyOps[X](val next:X, val route:Vector[String], val value:Int)

  trait Route {
    def __route:Vector[String]
  }

  trait Fields extends Route{ self =>

    private type ARG = self.type => (Vector[String], Bson)

    def int(name:String)         = new Primitive[Int](name)
    def double(name:String)      = new Primitive[Double](name)
    def long(name:String)        = new Primitive[Long](name)
    def string(name:String)      = new Primitive[String](name)
    def boolean(name:String)     = new Primitive[Boolean](name)
    def geolocation(name:String) = new Primitive[(Int, Int)](name)

    // query
    def $or(or:ARG*) =
      Fields.bsonArray[self.type]("$or", self, or)
    def $nor(nor:ARG*) =
      Fields.bsonArray[self.type]("$nor", self, nor)
    def $and(and:ARG*) =
      Fields.bsonArray[self.type]("$and", self, and)
    def $where(where:String) =
      (__route :+ "$where", ToBson(where))

    // update
    def $set(set:ARG*) =
      Fields.bsonDocument[self.type]("$set", self, set)
    def $unset(unset:ARG*) =
      Fields.bsonDocument[self.type]("$unset", self, unset)
    def $inc(inc:ARG*) =
      Fields.bsonDocument[self.type]("$inc", self, inc)
    def $rename(rename:(self.type => Fields#Field[_], self.type => Fields#Field[_])*) =
      (__route :+ "$rename", Bson.document(rename.map{ case (from, to) => Fields.name(from(this)) -> ToBson(Fields.name(to(this))) }))
    def $push(push:ARG*) =
      Fields.bsonDocument[self.type]("$push", self, push)
    def $pushAll(pushAll:(self.type => (Vector[String], Bson))*) =
      Fields.bsonDocument[self.type]("$pushAll", self, pushAll)
    def $addToSet(addToSet:ARG*) =
      Fields.bsonDocument[self.type]("$addToSet", self, addToSet)
    def $pop(pop:ARG*) =
      Fields.bsonDocument[self.type]("$pop", self, pop)
    def $pull(pull:ARG*) =
      Fields.bsonDocument[self.type]("$pull", self, pull)
    def $pullAll(pullAll:ARG*) =
      Fields.bsonDocument[self.type]("$pullAll", self, pullAll)

    class Primitive[A] (parent:Vector[String], name:String)(implicit toBson:ToBson[A]) extends Field[Primitive[A]](parent, name){ primitive =>
      def this(name:String)(implicit toBson:ToBson[A]) = this(__route, name)

      protected def next(name:String) = new Primitive[A](name)
      private val ops                 = new Ops[this.type, A](this, __route)

      def apply(value:A)                                                = (__route, ToBson(value))
      def apply(value:Seq[A])(implicit ev:this.type <:< IsArray)        = (__route, Bson.array(value.map(ToBson(_))))
      def apply(regex:String, options:String)(implicit ev:A =:= String) = (__route, Bson.regex(regex, options))
      def apply(sub:(Ops[this.type, A] => (Vector[String], Bson))*) =
        (__route, Bson.document(sub.map(f => Fields.path(__route, f(ops)))))

      def $(value:A)(implicit ev:this.type <:< IsArray) = super.$.apply(value)
    }

    abstract class Embedded[X <: Embedded[X]](name:String) extends Field[X](__route, name) with Fields {
      embedded: { def copy(name:String):X } =>
      protected def next(name:String) = copy(name)

      def apply(sub:(this.type => (Vector[String], Bson))*) =
        (__route, Bson.document(sub.map(f => Fields.path(__route, f(this)))))
    }

    sealed abstract class Field[A <: Field[A]](parent:Vector[String], name:String) extends Route{
      protected def next(name:String):A
      def __route = parent :+ name

      def $(implicit ev:this.type <:< IsArray) = next(name + ".$")
      def apply(index:Int) = new ApplyOps[A](next(name + "." + index), __route, index)

      def $elemMatch(elemMatch:(this.type => (Vector[String], Bson))*)(implicit ev:this.type <:< IsArray) =
        Fields.bsonDocument[this.type]("$elemMatch", this, elemMatch)

      // TODO - Ops
      def $slice(slice:Int) = (__route :+ "$slice", ToBson(slice))
      def $slice(skip:Int, limit:Int) = (__route :+ "$slice", ToBson((skip, limit)))
    }
  }

  trait Collections { self: Schema =>
    case class Collection(name:String) {

      def find(fields:(self.type => (Vector[String], Bson))*) =
        Find[self.type](self, name, Bson.document(fields.map(f => Fields.path(f(self)))))

      def find(js:String) =
        FindJs[self.type](self, name, Bson.string(js))

      def findOne(fields:(self.type => (Vector[String], Bson))*) =
        FindOne(name, Bson.document(fields.map(f => Fields.path(f(self)))))

      def insert(a:(self.type => (Vector[String], Bson))*){}

      def update(q:(self.type => (Vector[String], Bson))*)(u:(self.type => (Vector[String], Bson))*) =
        Update(name, Bson.document(q.map(f => Fields.path(f(self)))), Bson.document(u.map(f => Fields.path(f(self)))))

      def remove{ sys.error("remove") }

      def ensureIndex(on:(self.type => (Vector[String], Bson))*) =
        EnsureIndex(name, Bson.document(on.map(f => Fields.path(f(self)))))

      def reIndex(){ sys.error("reIndex") }

      def save(value:(self.type => (Vector[String], Bson))*) =
        Save(name, Bson.document(value.map(f => Fields.path(f(self)))))
    }
  }

  trait IsArray

  def array[X](value:X) = Fields.array(value)

  trait Schema extends Fields with Collections {
    def __route = Vector.empty[String]
  }

  trait Finder[A <: Fields] {
    def from:A
    def sort(by:(A => (Vector[String], Bson))*){}
  }

  case class Find[A <: Schema](from:A, collection:String, query:BsonDocument) extends Finder[A] {
    override def toString = "db."+collection+".find("+ query +")"

    def apply(fields:(A => (Vector[String], Bson))*) = FindSubset[A](collection, query, Bson.document(fields.map(f => Fields.path(f(from)))))
  }

  case class FindJs[A <: Schema](from:A, collection:String, query:BsonString) extends Finder[A] {
    override def toString = "db."+collection+".find("+query+")"
  }

  case class FindSubset[A <: Fields](collection:String, query:BsonDocument, subset:BsonDocument){
    override def toString = "db."+collection+".find("+query+", "+subset+")"
  }

  case class FindOne(collection:String, query:BsonDocument){
    override def toString = "db."+collection+".findOne("+query+")"
  }

  case class Update(collection:String, query:BsonDocument, update:BsonDocument){
    override def toString = "db."+collection+".update("+query+", "+update+")"
  }

  case class Save(collection:String, document:BsonDocument){
    override def toString = "db."+collection+".save("+document+")"
  }

  case class EnsureIndex(collection:String, fields:BsonDocument){
    override def toString = "db."+collection+".ensureIndex("+fields+")"
  }
}




