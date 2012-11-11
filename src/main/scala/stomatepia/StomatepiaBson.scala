package stomatepia

trait StomatepiaBson {

  type Bson
  def Bson:BsonProvider

  trait BsonProvider {

    def double(value:Double):Bson                  //1
    def string(value:String):Bson                  //2
    def document(fields:Seq[(String, Bson)]):Bson  //3
    def array(elements:Seq[Bson]):Bson             //4
    def binary(data:Array[Byte]):Bson              //5
    def objectId(id:String):Bson                   //7
    def boolean(value:Boolean):Bson                //8
    def date(value:java.util.Date):Bson            //9
    def NULL:Bson                                  //10
    def regex(value:String, options:String):Bson   //11
    def javascript(value:String):Bson              //13
    def symbol(value:Symbol):Bson                  //14
    def jsWithScope(value:String, scope:Bson):Bson //15
    def int(value:Int):Bson                        //16
    def timestamp(time:Int, inc:Int):Bson          //17
    def long(value:Long):Bson                      //18
    def minKey:Bson                                //255
    def maxKey:Bson                                //127
  }

  trait ToBson[A]{
    def toBson(value:A):Bson
  }

  object ToBson {
    def apply[A : ToBson](value:A) = implicitly[ToBson[A]].toBson(value)

    def from[A](f:A => Bson):ToBson[A] = new ToBson[A]{
      def toBson(value: A) = f(value)
    }

    implicit def identity[A <: Bson] = from[A](a => a)

    implicit val boolean = from(Bson.boolean)
    implicit val int     = from(Bson.int)
    implicit val long    = from(Bson.long)
    implicit val double  = from(Bson.double)
    implicit val string  = from(Bson.string)
    implicit val symbol  = from(Bson.symbol)
    implicit def seq[A : ToBson] = from[Seq[A]](value => Bson.array(value.map(ToBson(_))))
    implicit def tuple2[A : ToBson, B : ToBson] = from[(A, B)]{
      case (a, b) => Bson.array(Seq(ToBson(a), ToBson(b)))
    }
    implicit def tuple4[A : ToBson, B : ToBson, C : ToBson, D : ToBson] = from[(A, B, C, D)] {
      case (a, b, c, d) => Bson.array(Seq(ToBson(a), ToBson(b), ToBson(c), ToBson(d)))
    }
  }
}
