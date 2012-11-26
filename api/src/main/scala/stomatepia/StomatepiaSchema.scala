package stomatepia

trait StomatepiaSchema {
  self:Stomatepia =>

  implicit def schemaOps[A <: Schema](schema:A) = new SchemaOps[A](schema)

  abstract class Schema(val schemaName:String) extends Fields { self =>
    def __route = Vector.empty[String]
  }

  class SchemaOps[A <: Schema](schema:A) {
    private type Q    = QueryOps[A] => DocField
    private val  Q    = new QueryOps[A](self)
    private type U    = UpdateOps[A] => DocField
    private val  U    = new UpdateOps[A](self)
    private type Self = A => DocField
    private def self = schema
    private def name = schema.schemaName

    private type DocField = (Vector[String], Bson)

    private def Document[O](me:O, fields:Seq[O => DocField]) =
      Bson.document(fields.map(f => Fields.path(f(me))))

    def find(fields:Q*) = Find[A, Bool#F, Bool#F, Bool#F, Bool#F, Bool#F](self, name, Document(Q, fields), None, None, None, None, false)

    def find(js:String) = FindJs[A](self, name, Bson.string(js))

    def findOne(fields:Q*) = FindOne(name, Document(Q, fields))

    def insert(i:Self*) = Insert(name, Document[A](self, i))

    def insertAll(all:Seq[Self]*) = InsertAll(name, all.map(fields => Document[A](self, fields)))

    def update(q:Q*)(u:U*) = Update(name, Document(Q, q), Document(U, u))

    def remove{ sys.error("remove") }

    def ensureIndex(on:Self*) = EnsureIndex(name, Document[A](self, on))

    def reIndex(){ sys.error("reIndex") }

    def save(fields:Self*) = Save(name, Document[A](self, fields))
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
