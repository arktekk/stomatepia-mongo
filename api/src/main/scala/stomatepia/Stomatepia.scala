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

    def bsonDocument[A <: Route, O <: Ops[A]](name:String, what:O, fields:Seq[O => (Vector[String], Bson)]) =
      (what.__route :+ name, Bson.document(fields.map(f => path(what.__route, f(what)))))

    def bsonArray[A <: Route, O <: Ops[A]](name:String, what:O, fields:Seq[O => (Vector[String], Bson)]) =
      (what.__route :+ name, Bson.array(fields.map(f => Bson.document(Seq(path(what.__route, f(what)))))))
  }

  /*
  needs to be directly available instead of in Ops companion to beat any2Ensuring and any2ArrowAssoc for values called "x"
   */
  implicit def unwrapOps[X <: Route](ops:Ops[X]):X = ops.__underlying

  trait Ops[X <: Route]{
    val __underlying:X
  }

  class FieldOps[X <: Route, A : ToBson](val __underlying:X) extends Ops[X]{
    private def route = __underlying.__route
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
    def $within(within:(Within.type => (Vector[String], Bson))*)(implicit ev: A =:= (Int, Int)) =
      (route :+ "$within", Bson.document(within.map(w => Fields.path(route :+ "$within", w(Within)))))

    object Within {
      private val within = route :+ "$within"
      def $box(bottomLeft:(Int, Int), topRight:(Int, Int)) = (within :+ "$box", ToBson((bottomLeft, topRight)))
      def $box(a:(Int, Int), b:(Int, Int), c:(Int, Int), d:(Int, Int)) = (within :+ "$box", ToBson((a, b, c, d)))
      def $center(center:(Int, Int), radius:Int) = (within :+ "$center", ToBson((center, radius)))
      def $polygon(points:(Int, Int)*) = (within :+ "$polygon", ToBson(points))
      def $uniqueDocs(uniqueDocs:Boolean) = (within :+ "$uniqueDocs", ToBson(uniqueDocs))
    }

    //update
    def $each(values:Seq[A]) = operator("$each", values)

    //find keys
    def $slice(slice:Int)(implicit ev:X <:< IsArray) = operator("$slice", slice)
    def $slice(skip:Int, limit:Int)(implicit ev:X <:< IsArray) = operator("$slice", (skip, limit))
  }

  object ApplyOps {
    implicit def toBson[A <: Route](a:ApplyOps[A]) = (a.__route, ToBson(a.value))
  }

  class ApplyOps[X <: Route](val __underlying:X, val __route:Vector[String], val value:Int) extends Ops[X]

  class QueryOps[X <: Route](val __underlying:X) extends Ops[X]{
    private type ARG = QueryOps[X] => (Vector[String], Bson)
    private val __route = __underlying.__route

    // query
    def $or(or:ARG*) =
      Fields.bsonArray[X, this.type]("$or", this, or)
    def $nor(nor:ARG*) =
      Fields.bsonArray[X, this.type]("$nor", this, nor)
    def $and(and:ARG*) =
      Fields.bsonArray[X, this.type]("$and", this, and)
    def $where(where:String) =
      (__route :+ "$where", ToBson(where))
  }

  class UpdateOps[X <: Route](val __underlying:X) extends Ops[X] {
    private type ARG = UpdateOps[X] => (Vector[String], Bson)
    private val __route = __underlying.__route

    // update
    def $set(set:ARG*) =
      Fields.bsonDocument[X, this.type]("$set", this, set)
    def $unset(unset:ARG*) =
      Fields.bsonDocument[X, this.type]("$unset", this, unset)
    def $inc(inc:ARG*) =
      Fields.bsonDocument[X, this.type]("$inc", this, inc)
    def $rename(rename:(X => Fields#Field[_], X => Fields#Field[_])*) =
      (__route :+ "$rename", Bson.document(rename.map{ case (from, to) => Fields.name(from(this)) -> ToBson(Fields.name(to(this))) }))
    def $push(push:ARG*) =
      Fields.bsonDocument[X, this.type]("$push", this, push)
    def $pushAll(pushAll:ARG*) =
      Fields.bsonDocument[X, this.type]("$pushAll", this, pushAll)
    def $addToSet(addToSet:ARG*) =
      Fields.bsonDocument[X, this.type]("$addToSet", this, addToSet)
    def $pop(pop:ARG*) =
      Fields.bsonDocument[X, this.type]("$pop", this, pop)
    def $pull(pull:ARG*) =
      Fields.bsonDocument[X, this.type]("$pull", this, pull)
    def $pullAll(pullAll:ARG*) =
      Fields.bsonDocument[X, this.type]("$pullAll", this, pullAll)
  }


  trait Route {
    def __route:Vector[String]
  }

  trait Fields extends Route { self =>

    def bson[A : ToBson](name:String) = new Primitive[A](name)

    def int(name:String)         = bson[Int](name)
    def double(name:String)      = bson[Double](name)
    def long(name:String)        = bson[Long](name)
    def string(name:String)      = bson[String](name)
    def boolean(name:String)     = bson[Boolean](name)
    def geolocation(name:String) = bson[(Int, Int)](name)
    def date(name:String)        = bson[java.util.Date](name)

    class Primitive[A] (parent:Vector[String], name:String)(implicit toBson:ToBson[A]) extends Field[Primitive[A]](parent, name){ primitive =>
      def this(name:String)(implicit toBson:ToBson[A]) = this(__route, name)

      protected def next(name:String) = new Primitive[A](name)
      private val ops                 = new FieldOps[this.type, A](this)

      def apply(value:A)                                                = (__route, ToBson(value))
      def apply(value:Seq[A])(implicit ev:this.type <:< IsArray)        = (__route, Bson.array(value.map(ToBson(_))))
      def apply(regex:String, options:String)(implicit ev:A =:= String) = (__route, Bson.regex(regex, options))
      def apply(sub:(FieldOps[this.type, A] => (Vector[String], Bson))*) =
        (__route, Bson.document(sub.map(f => Fields.path(__route, f(ops)))))

      def $(value:A)(implicit ev:this.type <:< IsArray) = super.$.apply(value)
    }

    abstract class Embedded[X <: Embedded[X]](name:String) extends Field[X](__route, name) with Fields {
      embedded: { def copy(name:String):X } =>
      protected def next(name:String) = copy(name)

      def apply(sub:(this.type => (Vector[String], Bson))*) =
        (__route, Bson.document(sub.map(f => Fields.path(__route, f(this)))))

      def $elemMatch(elemMatch:(this.type => (Vector[String], Bson))*)(implicit ev:this.type <:< IsArray) =
        (__route :+ "$elemMatch", Bson.document(elemMatch.map(f => Fields.path(__route, f(this)))))

      // TODO - Ops ?
      def $slice(slice:Int)(implicit ev:this.type <:< IsArray) = (__route :+ "$slice", ToBson(slice))
      def $slice(skip:Int, limit:Int)(implicit ev:this.type <:< IsArray) = (__route :+ "$slice", ToBson((skip, limit)))
    }

    sealed abstract class Field[A <: Field[A]](parent:Vector[String], name:String) extends Route {
      protected def next(name:String):A
      def __route = parent :+ name

      def $(implicit ev:this.type <:< IsArray) = next(name + ".$")
      def apply(index:Int) = new ApplyOps[A](next(name + "." + index), __route, index)
    }
  }

  trait IsArray

  def array[X](value:X) = Fields.array(value)

  trait Schema extends Fields { self =>
    def __route = Vector.empty[String]

    case class Collection(name:String) {

      private type Q    = QueryOps[self.type] => DocField
      private val  Q    = new QueryOps[self.type](self)
      private type U    = UpdateOps[self.type] => DocField
      private val  U    = new UpdateOps[self.type](self)
      private type Self = self.type => DocField

      def find(fields:Q*) = Find[self.type, Bool#F, Bool#F, Bool#F, Bool#F, Bool#F](self, name, Document(Q, fields), None, None, None, None, false)

      def find(js:String) = FindJs[self.type](self, name, Bson.string(js))

      def findOne(fields:Q*) = FindOne(name, Document(Q, fields))

      def insert(i:Self*) = Insert(name, Document[self.type](self, i))

      def insertAll(all:Seq[Self]*) = InsertAll(name, all.map(fields => Document[self.type](self, fields)))

      def update(q:Q*)(u:U*) = Update(name, Document(Q, q), Document(U, u))

      def remove{ sys.error("remove") }

      def ensureIndex(on:Self*) = EnsureIndex(name, Document[self.type](self, on))

      def reIndex(){ sys.error("reIndex") }

      def save(fields:Self*) = Save(name, Document[self.type](self, fields))
    }

    private type DocField = (Vector[String], Bson)

    private def Document[O](me:O, fields:Seq[O => DocField]) =
      Bson.document(fields.map(f => Fields.path(f(me))))
  }

  sealed trait Bool {
    sealed trait T extends Bool
    sealed trait F extends Bool
  }

  // model this as traits with Optional values + phantom types to track usage/state
  case class Find[A <: Schema, KEYS <: Bool, SORTED <: Bool, LIMIT <: Bool, SKIP <: Bool, SNAPSHOT <: Bool](from:A, collection:String, query:BsonDocument, keys:Option[BsonDocument], sorted:Option[BsonDocument], limits:Option[Int], skips:Option[Int], snapshots:Boolean) {
    override def toString = "db."+collection+".find("+ query + keys.map(", " +).getOrElse("") +")" + sorted.map(s => ".sort("+s+")").getOrElse("")

    def count = Count(this)

    def limit(value:Int)(implicit ev:LIMIT =:= Bool#F) =
      Find[A, KEYS, SORTED, Bool#T, SKIP, SNAPSHOT](from, collection, query, keys, sorted, Some(value), skips, snapshots)
    def skip(value:Int)(implicit ev:SKIP =:= Bool#F) =
      Find[A, KEYS, SORTED, LIMIT, Bool#T, SNAPSHOT](from, collection, query, keys, sorted, limits, Some(value), snapshots)
    // may not be used with sorting or explicit hints
    def snapshot(implicit ev:SNAPSHOT =:= Bool#F, ev1:SORTED =:= Bool#F) =
      Find[A, KEYS, SORTED, LIMIT, SKIP, Bool#T](from, collection, query, keys, sorted, limits, skips, true)
//    def batchSize()

    def sort(by:(A => (Vector[String], Bson))*)(implicit ev: SORTED =:= Bool#F) =
      Find[A, KEYS, Bool#T, LIMIT, SKIP, SNAPSHOT](from, collection, query, keys, Some(Bson.document(by.map(f => Fields.path(f(from))))), limits, skips, snapshots)

    def apply(fields:(A => (Vector[String], Bson))*)(implicit ev:KEYS =:= Bool#F) =
      Find[A, Bool#T, SORTED, LIMIT, SKIP, SNAPSHOT](from, collection, query, Some(Bson.document(fields.map(f => Fields.path(f(from))))), sorted, limits, skips, snapshots)
  }

  case class Count(find:Find[_ <: Schema, _ <: Bool, _ <: Bool, _ <: Bool, _ <: Bool, _ <: Bool]){
    override def toString = find.toString+".count()"
  }

  case class FindJs[A <: Schema](from:A, collection:String, query:BsonString) {
    override def toString = "db."+collection+".find("+query+")"
  }

  case class FindOne(collection:String, query:BsonDocument){
    override def toString = "db."+collection+".findOne("+query+")"
  }

  case class Insert(collection:String, document:BsonDocument){
    override def toString = "db."+collection+".insert("+document+")"
  }

  case class InsertAll(collection:String, documents:Seq[BsonDocument]){
    override def toString = "db."+collection+".insert("+documents.mkString("[ ",", ", " ]")+")"
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




