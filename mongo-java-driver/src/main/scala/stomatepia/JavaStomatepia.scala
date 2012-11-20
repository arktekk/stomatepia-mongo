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

    def cursor(find:Find[_]) = {
      val collection = db.getCollection(find.collection)
      collection.find(find.query)
    }

    def cursor(find:FindKeys[_]) = {
      db.getCollection(find.collection).find(find.query, find.keys)
    }

    def apply[A](f:this.type => A) = f(this)
  }

  class ExecuteFind(db:StomatepiaDb, find:Find[_]) extends Traversable[DBObject]{
    def foreach[U](f: (DBObject) => U) {
      StomatepiaDb.foreach(f, db.cursor(find))
    }
  }

  class ExecuteFindKeys(db:StomatepiaDb, find:FindKeys[_]) extends Traversable[DBObject]{
    def foreach[U](f: (DBObject) => U) {
      StomatepiaDb.foreach(f, db.cursor(find))
    }
  }

  implicit def executeFind(find:Find[_])(implicit db:StomatepiaDb) = new ExecuteFind(db, find)
  implicit def executeFindKeys(findKeys:FindKeys[_])(implicit db:StomatepiaDb) = new ExecuteFindKeys(db, findKeys)
}