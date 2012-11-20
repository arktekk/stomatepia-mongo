package stomatepia

import com.mongodb.{Mongo, DBCursor, DBObject, DB}
import collection.generic.CanBuildFrom

object JavaStomatepia extends JavaStomatepia

trait JavaStomatepia extends Stomatepia with JavaBson {
  object StomatepiaDb {
    def to[C[_]](cursor:DBCursor)(implicit can:CanBuildFrom[Nothing, DBObject, C[DBObject]]):C[DBObject] = {
      val builder = can.apply()
      foreach(builder +=, cursor)
      builder.result()
    }

    def foreach(f:DBObject => Any, cursor:DBCursor){
      try{
        while(cursor.hasNext){
          f(cursor.next)
        }
      } finally {
        cursor.close()
      }
    }
  }

  class StomatepiaDb(db:DB) {

    def cursor(find:Find[_, _, _, _, _, _]) = {
      val collection = db.getCollection(find.collection)
      val c = collection.find(find.query, find.keys.orNull)
      find.sorted.foreach(c.sort)
      find.limits.foreach(c.limit)
      find.skips.foreach(c.skip)
      if(find.snapshots)
        c.snapshot()
      c
    }

    def apply[A](f:this.type => A) = f(this)
  }

  class ExecuteFind(db:StomatepiaDb, find:Find[_, _, _, _, _, _]) extends Traversable[DBObject]{
    def foreach[U](f: (DBObject) => U) {
      StomatepiaDb.foreach(f, db.cursor(find))
    }
  }

  implicit def executeFind(find:Find[_, _, _, _, _, _])(implicit db:StomatepiaDb) =
    new ExecuteFind(db, find)
  implicit def executeCount(count:Count)(implicit db:StomatepiaDb) =
    db.cursor(count.find).count()
}