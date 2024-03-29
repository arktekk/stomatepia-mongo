package stomatepia

class AdvancedQueries extends StomatepiaSuite {

  object db {
    object things extends Schema("things") {
      val x = int("x")
      val y = string("y")
      val j = int("j")
      val k = int("k")
      val a = array(int("a"))
      val colors = array(string("colors"))
    }

    object collection extends Schema("collection") {
      val field = int("field")
    }

    object foo extends Schema("foo") {
      val a = array(int("a"))
      val b = int("b")
      val name = string("name")
    }

    object customers extends Schema("customers") {
      val name = string("name")
    }

    object x extends Schema("x") {
      val someId = string("someId")
    }

    object t extends Schema("t") {
      val x = array(X("x"))
      val x2 = array(int("x"))

      case class X(name:String) extends Embedded[X](name){
        val a = int("a")
        val b = int("b")
      }
    }

    object postings extends Schema("postings") {
      val author = Author("author")

      case class Author(_name:String) extends Embedded[Author](_name){
        val name = string("name")
      }
    }

    object myCollection extends Schema("myCollection") {
      val a = int("a")
      val registered = boolean("registered")
    }
  }

    db.things.find(_.x(3), _.y("foo")) is """db.things.find({ "x" : 3, "y" : 'foo' })"""

    db.things.find(_.j(_.$ne(3)), _.k(_.$gt(10)))    is """db.things.find({ "j" : { "$ne" : 3 }, "k" : { "$gt" : 10 } })"""

    db.things.insert(_.colors(Seq("blue", "black")))

    db.things.insert(_.colors(Seq("yellow", "orange", "red")))

    db.things.find(_.colors(_.$ne("red")))       is """db.things.find({ "colors" : { "$ne" : 'red' } })"""

  // TODO retrieving a keys of fields

    val value = 1

    db.collection.find(_.field(_.$gt(value)))  is """db.collection.find({ "field" : { "$gt" : 1 } })"""
    db.collection.find(_.field(_ > value))    is """db.collection.find({ "field" : { "$gt" : 1 } })"""

    db.collection.find(_.field(_.$lt(value)))  is """db.collection.find({ "field" : { "$lt" : 1 } })"""
    db.collection.find(_.field(_ < value))    is """db.collection.find({ "field" : { "$lt" : 1 } })"""

    db.collection.find(_.field(_.$gte(value))) is """db.collection.find({ "field" : { "$gte" : 1 } })"""
    db.collection.find(_.field(_ >= value))   is """db.collection.find({ "field" : { "$gte" : 1 } })"""

    db.collection.find(_.field(_.$lte(value))) is """db.collection.find({ "field" : { "$lte" : 1 } })"""
    db.collection.find(_.field(_ <= value))   is """db.collection.find({ "field" : { "$lte" : 1 } })"""

    db.things.find(_.a(_.$all(Seq(2, 3))))    is """db.things.find({ "a" : { "$all" : [ 2, 3 ] } })"""

    db.things.find(_.a(_.$all(Seq(2, 3, 4)))) is """db.things.find({ "a" : { "$all" : [ 2, 3, 4 ] } })"""

    db.things.find(_.a(_.$exists(true)))      is """db.things.find({ "a" : { "$exists" : true } })"""

    db.things.find(_.a(_.$exists(false)))     is """db.things.find({ "a" : { "$exists" : false } })"""

    db.things.find("this.a % 10 == 1")    is """db.things.find('this.a % 10 == 1')"""

    db.things.find(_.a(_.$mod(10, 1)))      is """db.things.find({ "a" : { "$mod" : [ 10, 1 ] } })"""

    db.things.find(_.x(_.$ne(3)))             is """db.things.find({ "x" : { "$ne" : 3 } })"""

    val array = Seq(1, 2, 3)
    db.collection.find(_.field(_.$in(array))) is """db.collection.find({ "field" : { "$in" : [ 1, 2, 3 ] } })"""

    db.things.find(_.j(_.$in(Seq(2, 4, 6))))  is """db.things.find({ "j" : { "$in" : [ 2, 4, 6 ] } })"""

    db.things.find(_.j(_.$nin(Seq(2, 4, 6)))) is """db.things.find({ "j" : { "$nin" : [ 2, 4, 6 ] } })"""

    db.foo.find(_.$or(_.a(1), _.b(2)))                   is """db.foo.find({ "$or" : [ { "a" : 1 }, { "b" : 2 } ] })"""

    db.foo.find(_.name("bob"), _.$or(_.a(1), _.b(2))) is """db.foo.find({ "name" : 'bob', "$or" : [ { "a" : 1 }, { "b" : 2 } ] })"""

    db.foo.insert(_.a(Seq(1, 10)))

    db.foo.find(_.$and(_.a(1), _.a(_.$gt(5)))) is """db.foo.find({ "$and" : [ { "a" : 1 }, { "a" : { "$gt" : 5 } } ] })"""

    db.things.find(_.a(_.$size(1))) is """db.things.find({ "a" : { "$size" : 1 } })"""

    db.things.find(_.a(_.$type(2)))  is """db.things.find({ "a" : { "$type" : 2 } })"""
    db.things.find(_.a(_.$type(16))) is """db.things.find({ "a" : { "$type" : 16 } })"""

    // TODO, min.shardKey ?

    db.customers.find(_.name(_.$regex("acme.*corp"), _.$options("i"))) is """db.customers.find({ "name" : { "$regex" : 'acme.*corp', "$options" : 'i' } })"""

    db.customers.find(_.name(_.$regex("acme.*corp"), _.$options("i"), _.$nin(Seq("acmeblahcorp")))) is """db.customers.find({ "name" : { "$regex" : 'acme.*corp', "$options" : 'i', "$nin" : [ 'acmeblahcorp' ] } })"""

    db.x.insert(_.someId("123[456]"))
    db.x.find(_.someId(_.$regex("123\\[456\\]"))) is """db.x.find({ "someId" : { "$regex" : '123\\[456\\]' } })"""

    db.things.find(_.colors("red")) is """db.things.find({ "colors" : 'red' })"""

    db.t.find(_.x(_.$elemMatch(_.a(1), _.b(_.$gt(1))))) is """db.t.find({ "x" : { "$elemMatch" : { "a" : 1, "b" : { "$gt" : 1 } } } })"""

    db.t.find(_.x.a(1), _.x.b(_.$gt(1)))              is """db.t.find({ "x.a" : 1, "x.b" : { "$gt" : 1 } })"""

    db.t.find(_.x2(_.$gt(2), _.$lt(5)))              is """db.t.find({ "x" : { "$gt" : 2, "$lt" : 5 } })"""

    db.t.find(_.x2(_.$elemMatch(_.$gt(2), _.$lt(5))))  is """db.t.find({ "x" : { "$elemMatch" : { "$gt" : 2, "$lt" : 5 } } })"""

    db.postings.find(_.author.name("joe")) is """db.postings.find({ "author.name" : 'joe' })"""

    db.customers.find(_.name(_.$not(_.$regex("acme.*corp"), _.$options("i")))) is """db.customers.find({ "name" : { "$not" : { "$regex" : 'acme.*corp', "$options" : 'i' } } })"""

    db.things.find(_.a(_.$not(_.$mod(10, 1)))) is """db.things.find({ "a" : { "$not" : { "$mod" : [ 10, 1 ] } } })"""

    db.myCollection.find(_.a(_.$gt(3)))                               is """db.myCollection.find({ "a" : { "$gt" : 3 } })"""
    db.myCollection.find(_.$where("this.a > 3"))                      is """db.myCollection.find({ "$where" : 'this.a > 3' })"""
    db.myCollection.find("this.a > 3")                                is """db.myCollection.find('this.a > 3')"""
    db.myCollection.find("function(){ return this.a > 3; }")          is """db.myCollection.find('function(){ return this.a > 3; }')"""

    db.myCollection.find(_.registered(true), _.$where("this.a>3")) is """db.myCollection.find({ "registered" : true, "$where" : 'this.a>3' })"""
}
