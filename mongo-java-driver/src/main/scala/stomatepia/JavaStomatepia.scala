package stomatepia

import com.mongodb.{DBCursor, DBObject, DB}
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

    def cursor[A <: Schema](find:Find[A]) = {
      val collection = db.getCollection(find.collection)
      collection.find(find.query)
    }

    def cursor[A <: Schema](find:FindKeys[A]) = {
      db.getCollection(find.collection).find(find.query, find.keys)
    }

    def apply[A](f:this.type => A) = f(this)
  }

  class ExecuteFind[A <: Schema](find:Find[A]){
    def to[C[_]](implicit db:StomatepiaDb, can:CanBuildFrom[Nothing, DBObject, C[DBObject]]) =
      StomatepiaDb.to[C](db.cursor(find))
    def foreach(f:DBObject => Any)(implicit db:StomatepiaDb){
      StomatepiaDb.foreach(f, db.cursor(find))
    }
  }

  class ExecuteFindKeys[A <: Schema](findKeys:FindKeys[A]){
    def to[C[_]](implicit db:StomatepiaDb, can:CanBuildFrom[Nothing, DBObject, C[DBObject]]) =
      StomatepiaDb.to[C](db.cursor(findKeys))
    def foreach(f:DBObject => Any)(implicit db:StomatepiaDb){
      StomatepiaDb.foreach(f, db.cursor(findKeys))
    }
  }

  implicit def executeFind[A <: Schema](find:Find[A]) = new ExecuteFind[A](find)
  implicit def executeFindKeys[A <: Schema](findKeys:FindKeys[A]) = new ExecuteFindKeys[A](findKeys)
}
