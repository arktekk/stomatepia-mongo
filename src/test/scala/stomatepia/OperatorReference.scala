package stomatepia

class OperatorReference extends StomatepiaSuite { self =>

  object Inventory extends Schema {
    val qty     = int("qty")
    val carrier = Carrier("carrier")
    val price   = double("price")
    val tags    = array(string("tags"))
    val sale    = boolean("sale")
    val item    = string("item")

    case class Carrier(name:String) extends Embedded[Carrier](name){
      val state = string("state")
      val fee   = int("fee")
    }
  }

  object Collection extends Schema {
    val field    = self.array(string("field"))
    val field1   = self.array(string("field1"))
    val location = geolocation("location")
    val loc      = geolocation("loc")
    val array    = self.array(Fields("array"))
    val age      = int("age")
    val name     = string("name")

    case class Fields(name:String) extends Embedded[Fields](name){
      val value1 = int("value1")
      val value2 = int("value2")
    }
  }

  object Addressbook extends Schema {
    val addresses = Addresses("addresses")
    val address   = Addresses("address")

    case class Addresses(name:String) extends Embedded[Addresses](name){
      val loc = geolocation("loc")
    }
  }

  object Student extends Schema {
    val _id      = int("_id")
    val nickname = string("nickname")
    val alias    = string("alias")
    val cell     = string("cell")
    val mobile   = string("mobile")
    val nmae     = string("nmae")
    val name     = string("name")
    val wife     = string("wife")
    val spouse   = string("spouse")
    val vice     = string("vice")
    val vp       = string("vp")
    val office   = string("office")
    val term     = string("term")
    val grades   = array(int("grades"))
  }

  object Student2 extends Schema {
    val _id = int("_id")
    val name = Name("name")
    val contact = Contact("contact")
    val grades = array(Grade("grades"))

    case class Name(name:String) extends Embedded[Name](name){
      val first = string("first")
      val fname = string("fname")
      val last  = string("last")
    }

    case class Contact(name:String) extends Embedded[Contact](name){
      val lname = string("lname")
    }

    case class Grade(name:String) extends Embedded[Grade](name){
      val grade = int("grade")
      val std   = int("std")
    }
  }

  object Post extends Schema {
    val comments = array(string("comments"))
  }

  object db {
    val inventory   = Inventory.Collection("inventory")
    val collection  = Collection.Collection("collection")
    val addressBook = Addressbook.Collection("addressBook")
    val students    = Student.Collection("students")
    val students2   = Student2.Collection("students")
    val posts       = Post.Collection("posts")
  }

  db.inventory.find(_.qty(_.$ne(20))) is """db.inventory.find({ "qty" : { "$ne" : 20 } })"""

  db.inventory.update(_.carrier.state(_.$ne("NY")))(_.$set(_.qty(20))) is """db.inventory.update({ "carrier.state" : { "$ne" : 'NY' } }, { "$set" : { "qty" : 20 } })"""

  //$lt
  db.inventory.find(_.qty(_.$lt(20))) is """db.inventory.find({ "qty" : { "$lt" : 20 } })"""

  db.inventory.update(_.carrier.fee(_.$lt(20)))(_.$set(_.price(9.99))) is """db.inventory.update({ "carrier.fee" : { "$lt" : 20 } }, { "$set" : { "price" : 9.99 } })"""

  //$lte
  db.inventory.find(_.qty(_.$lte(20))) is """db.inventory.find({ "qty" : { "$lte" : 20 } })"""

  db.inventory.update(_.carrier.fee(_.$lte(5)))(_.$set(_.price(9.99))) is """db.inventory.update({ "carrier.fee" : { "$lte" : 5 } }, { "$set" : { "price" : 9.99 } })"""

  //$gt
  db.inventory.find(_.qty(_.$gt(20))) is """db.inventory.find({ "qty" : { "$gt" : 20 } })"""

  db.inventory.update(_.carrier.fee(_.$gt(2)))(_.$set(_.price(9.99))) is """db.inventory.update({ "carrier.fee" : { "$gt" : 2 } }, { "$set" : { "price" : 9.99 } })"""

  //$gte
  db.inventory.find(_.qty(_.$gte(20))) is """db.inventory.find({ "qty" : { "$gte" : 20 } })"""

  db.inventory.update(_.carrier.fee(_.$gte(2)))(_.$set(_.price(9.99))) is """db.inventory.update({ "carrier.fee" : { "$gte" : 2 } }, { "$set" : { "price" : 9.99 } })"""

  //$in
  db.inventory.find(_.qty(_.$in(Seq(5, 15)))) is """db.inventory.find({ "qty" : { "$in" : [ 5, 15 ] } })"""

  db.inventory.update(_.tags(_.$in(Seq("appliances", "school"))))(_.$set(_.sale(true))) is """db.inventory.update({ "tags" : { "$in" : [ 'appliances', 'school' ] } }, { "$set" : { "sale" : true } })"""

  //$nin
  db.inventory.find(_.qty(_.$nin(Seq(5, 15)))) is """db.inventory.find({ "qty" : { "$nin" : [ 5, 15 ] } })"""

  db.inventory.update(_.tags(_.$nin(Seq("appliances", "school"))))(_.$set(_.sale(false))) is """db.inventory.update({ "tags" : { "$nin" : [ 'appliances', 'school' ] } }, { "$set" : { "sale" : false } })"""

  //$all
  db.inventory.find(_.tags(_.$all(Seq("appliances", "school", "book")))) is """db.inventory.find({ "tags" : { "$all" : [ 'appliances', 'school', 'book' ] } })"""

  // is actually allowed by mongo (means the same as db.inventory.find( { qty: 50 } )). should is be allowed or not ?
  //  db.inventory.find(_.qty(_.$all(Seq(50)))) is """db.inventory.find({ "qty" : { "$all" : [ 50 ] } })"""

  // ---- Logical ----
  // $and
  db.inventory.find(_.$and(_.price(1.99), _.qty(_.$lt(20)), _.sale(true))) is """db.inventory.find({ "$and" : [ { "price" : 1.99 }, { "qty" : { "$lt" : 20 } }, { "sale" : true } ] })"""

  db.inventory.find(_.price(1.99), _.qty(_.$lt(20)), _.sale(true)) is """db.inventory.find({ "price" : 1.99, "qty" : { "$lt" : 20 }, "sale" : true })"""

  db.inventory.update(_.$and(_.price(_.$ne(1.99)), _.price(_.$exists(true))))(_.$set(_.qty(15))) is """db.inventory.update({ "$and" : [ { "price" : { "$ne" : 1.99 } }, { "price" : { "$exists" : true } } ] }, { "$set" : { "qty" : 15 } })"""

  db.inventory.update(_.price(_.$ne(1.99), _.$exists(true)))(_.$set(_.qty(15))) is """db.inventory.update({ "price" : { "$ne" : 1.99, "$exists" : true } }, { "$set" : { "qty" : 15 } })"""

  //$or
  db.inventory.find(_.price(1.99), _.$or(_.qty(_.$lt(20)), _.sale(true))) is """db.inventory.find({ "price" : 1.99, "$or" : [ { "qty" : { "$lt" : 20 } }, { "sale" : true } ] })"""

  db.inventory.update(_.$or(_.price(10.99), _.carrier.state("NY")))(_.$set(_.sale(true))) is """db.inventory.update({ "$or" : [ { "price" : 10.99 }, { "carrier.state" : 'NY' } ] }, { "$set" : { "sale" : true } })"""

  db.inventory.find(_.$or(_.price(1.99), _.sale(true)), _.qty(_.$in(Seq(20, 50)))) is """db.inventory.find({ "$or" : [ { "price" : 1.99 }, { "sale" : true } ], "qty" : { "$in" : [ 20, 50 ] } })"""

  db.inventory.find(_.$or(_.price(1.99), _.sale(true))) is """db.inventory.find({ "$or" : [ { "price" : 1.99 }, { "sale" : true } ] })"""

  db.inventory.ensureIndex(_.price(1)) // TODO test

  db.inventory.ensureIndex(_.sale(1)) // TODO test

  db.inventory.find(_.$or(_.price(1.99), _.sale(true))).sort(_.item(1))

  //$nor
  db.inventory.find(_.$nor(_.price(1.99), _.qty(_.$lt(20)), _.sale(true))) is """db.inventory.find({ "$nor" : [ { "price" : 1.99 }, { "qty" : { "$lt" : 20 } }, { "sale" : true } ] })"""

  db.inventory.find(_.$nor(_.price(1.99), _.sale(true))) is """db.inventory.find({ "$nor" : [ { "price" : 1.99 }, { "sale" : true } ] })"""

  db.inventory.find(_.$nor(_.price(1.99), _.price(_.$exists(false)), _.sale(true), _.sale(_.$exists(false)))) is """db.inventory.find({ "$nor" : [ { "price" : 1.99 }, { "price" : { "$exists" : false } }, { "sale" : true }, { "sale" : { "$exists" : false } } ] })"""

  //$not
  db.inventory.find(_.price(_.$not(_.$gt(1.99)))) is """db.inventory.find({ "price" : { "$not" : { "$gt" : 1.99 } } })"""

  db.inventory.find(_.item(_.$not("^p.*"))) is """db.inventory.find({ "item" : { "$not" : /^p.*/ } })"""

  //$exists
  db.inventory.find(_.qty(_.$exists(true), _.$nin(Seq(5, 15)))) is """db.inventory.find({ "qty" : { "$exists" : true, "$nin" : [ 5, 15 ] } })"""

  //$type
  db.inventory.find(_.price(_.$type(1))) is """db.inventory.find({ "price" : { "$type" : 1 } })"""

  db.inventory.find(_.tags(_.$type(4))) is """db.inventory.find({ "tags" : { "$type" : 4 } })"""

  db.inventory.find(_.$where("Array.isArray(this.tags)")) is """db.inventory.find({ "$where" : 'Array.isArray(this.tags)' })"""

  db.inventory.find(_.qty(_.$mod(4, 0))) is """db.inventory.find({ "qty" : { "$mod" : [ 4, 0 ] } })"""

  db.inventory.find(_.qty(_.$mod(4, 3))) is """db.inventory.find({ "qty" : { "$mod" : [ 4, 3 ] } })"""

  db.inventory.find(_.$where("this.qty % 4 == 3")) is """db.inventory.find({ "$where" : 'this.qty % 4 == 3' })"""

  //$where
  db.collection.find(_.$where("this.a == this.b")) is """db.collection.find({ "$where" : 'this.a == this.b' })"""

  //$regex
  db.collection.find(_.field("acme.*corp", "i")) is """db.collection.find({ "field" : /acme.*corp/i })"""

  db.collection.find(_.field(_.$regex("acme.*corp"), _.$options("i"))) is """db.collection.find({ "field" : { "$regex" : 'acme.*corp', "$options" : 'i' } })"""

  // --- GEOSPATIAL ---
  //$near
  db.collection.find(_.location(_.$near(100, 100))) is """db.collection.find({ "location" : { "$near" : [ 100, 100 ] } })"""

  // $within $box
  db.collection.find(_.location(_.$within(_.$box((100, 0), (120,100))))) is """db.collection.find({ "location" : { "$within" : { "$box" : [ [ 100, 0 ], [ 120, 100 ] ] } } })"""
  db.collection.find(_.loc(_.$within(_.$box((0,0), (100,100))))) is """db.collection.find({ "loc" : { "$within" : { "$box" : [ [ 0, 0 ], [ 100, 100 ] ] } } })"""
  db.collection.find(_.location(_.$within(_.$box((100,120), (100,100), (120,100), (240,200))))) is """db.collection.find({ "location" : { "$within" : { "$box" : [ [ 100, 120 ], [ 100, 100 ], [ 120, 100 ], [ 240, 200 ] ] } } })"""

  // $within $center
  db.collection.find(_.location(_.$within(_.$center((0,0), 10)))) is """db.collection.find({ "location" : { "$within" : { "$center" : [ [ 0, 0 ], 10 ] } } })"""

  // $within $polygon
  db.collection.find(_.loc(_.$within(_.$polygon((0,0),(3,6),(6,0))))) is """db.collection.find({ "loc" : { "$within" : { "$polygon" : [ [ 0, 0 ], [ 3, 6 ], [ 6, 0 ] ] } } })"""
//
  //$uniqueDocs
  db.addressBook.find(_.addresses.loc(_.$within(_.$box((0,0), (100,100)), _.$uniqueDocs(false)))) is """db.addressBook.find({ "addresses.loc" : { "$within" : { "$box" : [ [ 0, 0 ], [ 100, 100 ] ], "$uniqueDocs" : false } } })"""

  db.addressBook.find(_.address.loc(_.$within(_.$box((0,0), (100,100)), _.$uniqueDocs(true)))) is """db.addressBook.find({ "address.loc" : { "$within" : { "$box" : [ [ 0, 0 ], [ 100, 100 ] ], "$uniqueDocs" : true } } })"""

  //$maxDistance
  db.collection.find(_.location(_.$near(100,100), _.$maxDistance(10))) is """db.collection.find({ "location" : { "$near" : [ 100, 100 ], "$maxDistance" : 10 } })"""

  //$nearSphere
  db.collection.find(_.loc(_.$nearSphere(0,0))) is """db.collection.find({ "loc" : { "$nearSphere" : [ 0, 0 ] } })"""

  //$centerSphere
  db.collection.find(_.loc(_.$centerSphere((0,0), 10 / 3959))) is """db.collection.find({ "loc" : { "$centerSphere" : [ [ 0, 0 ], """+(10 / 3959)+" ] } })"

  // --- ARRAY ---

  //$size
  db.collection.find(_.field(_.$size(2))) is """db.collection.find({ "field" : { "$size" : 2 } })"""

  db.collection.find(_.field(_.$size(1))) is """db.collection.find({ "field" : { "$size" : 1 } })"""

  //$elemMatch
  db.collection.find(_.array(_.$elemMatch(_.value1(1), _.value2(_.$gt(1))))) is """db.collection.find({ "array" : { "$elemMatch" : { "value1" : 1, "value2" : { "$gt" : 1 } } } })"""

  // --- UPDATE ---
  // --- FIELDS ---

  //$set
  db.collection.update(_.field("value1"))(_.$set(_.field1("value2"))) is """db.collection.update({ "field" : 'value1' }, { "$set" : { "field1" : 'value2' } })"""

  //$unset
  db.collection.update(_.field("value1"))(_.$unset(_.field1(""))) is """db.collection.update({ "field" : 'value1' }, { "$unset" : { "field1" : '' } })"""

  //$inc

  db.collection.update(_.age(20))(_.$inc(_.age(1))) is """db.collection.update({ "age" : 20 }, { "$inc" : { "age" : 1 } })"""

  db.collection.update(_.name("John"))(_.$inc(_.age(1))) is """db.collection.update({ "name" : 'John' }, { "$inc" : { "age" : 1 } })"""

  //$rename
  db.students.update(_._id(1))(_.$rename((_.nickname, _.alias), (_.cell, _.mobile))) is """db.students.update({ "_id" : 1 }, { "$rename" : { "nickname" : 'alias', "cell" : 'mobile' } })"""

  db.students.update(_._id(1))(_.$rename((_.nmae, _.name))) is """db.students.update({ "_id" : 1 }, { "$rename" : { "nmae" : 'name' } })"""

  db.students2.update(_._id(1))(_.$rename((_.name.first, _.name.fname))) is """db.students.update({ "_id" : 1 }, { "$rename" : { "name.first" : 'name.fname' } })"""

  db.students2.update(_._id(1))(_.$rename((_.name.last, _.contact.lname))) is """db.students.update({ "_id" : 1 }, { "$rename" : { "name.last" : 'contact.lname' } })"""

  db.students.update(_._id(1))(_.$rename((_.wife, _.spouse))) is """db.students.update({ "_id" : 1 }, { "$rename" : { "wife" : 'spouse' } })"""

  db.students.update(_._id(1))(_.$rename((_.wife, _.spouse), (_.vice, _.vp), (_.office, _.term))) is """db.students.update({ "_id" : 1 }, { "$rename" : { "wife" : 'spouse', "vice" : 'vp', "office" : 'term' } })"""

  db.students.update(_._id(1))(_.$rename((_.wife, _.alias), (_.mobile, _.cell))) is """db.students.update({ "_id" : 1 }, { "$rename" : { "wife" : 'alias', "mobile" : 'cell' } })"""

  // --- ARRAY ---
  // $

  db.students.update(_._id(1), _.grades(80))(_.$set(_.grades.$(82))) is """db.students.update({ "_id" : 1, "grades" : 80 }, { "$set" : { "grades.$" : 82 } })"""

  db.students2.update(_._id(4), _.grades.grade(85))(_.$set(_.grades.$.std(6))) is """db.students.update({ "_id" : 4, "grades.grade" : 85 }, { "$set" : { "grades.$.std" : 6 } })"""

  // $push
  db.collection.update(_.field("value"))(_.$push(_.field("value1"))) is """db.collection.update({ "field" : 'value' }, { "$push" : { "field" : 'value1' } })"""

  // $pushAll
  db.collection.update(_.field("value"))(_.$pushAll(_.field1(Seq("value1", "value2", "value3")))) is """db.collection.update({ "field" : 'value' }, { "$pushAll" : { "field1" : [ 'value1', 'value2', 'value3' ] } })"""

  // $addToSet
  db.collection.update(_.field("value"))(_.$addToSet(_.field("value1"))) is """db.collection.update({ "field" : 'value' }, { "$addToSet" : { "field" : 'value1' } })"""
//
  // $addToSet.$each
  db.collection.update(_.field("value"))(_.$addToSet(_.field(_.$each(Seq("value1", "value2", "value3"))))) is """db.collection.update({ "field" : 'value' }, { "$addToSet" : { "field" : { "$each" : [ 'value1', 'value2', 'value3' ] } } })"""

  // $pop
  db.collection.update(_.field("value"))(_.$pop(_.field(1))) is """db.collection.update({ "field" : 'value' }, { "$pop" : { "field" : 1 } })"""

  db.collection.update(_.field("value"))(_.$pop(_.field(-1))) is """db.collection.update({ "field" : 'value' }, { "$pop" : { "field" : -1 } })"""

  // $pull
  db.collection.update(_.field("value"))(_.$pull(_.field("value1"))) is """db.collection.update({ "field" : 'value' }, { "$pull" : { "field" : 'value1' } })"""

  db.collection.update(_.field("value"))(_.$pullAll(_.field1(Seq("value1", "value2", "value3")))) is """db.collection.update({ "field" : 'value' }, { "$pullAll" : { "field1" : [ 'value1', 'value2', 'value3' ] } })"""
//
//  // --- BITWISE --
//  //$bit
//  db.collection.update( { field: 1 }, { $bit: { field: { and: 5 } } } ) is """db.collection.update( { field: 1 }, { $bit: { field: { and: 5 } } } )"""
//
//  // -- ISOLATION --
//  // $atomic
//  db.foo.update( { field1 : 1 , $atomic : 1 }, { $inc : { field2 : 1 } } ,  false , true ) is """db.foo.update( { field1 : 1 , $atomic : 1 }, { $inc : { field2 : 1 } } ,  false , true )"""
//
  // -- PROJECTION --
  //$slice
  db.collection.find(_.field("value"))(_.array(_.$slice(5))) is """db.collection.find({ "field" : 'value' }, { "array" : { "$slice" : 5 } })"""

  db.posts.find()(_.comments(_.$slice(5))) is """db.posts.find({}, { "comments" : { "$slice" : 5 } })"""

  db.posts.find()(_.comments(_.$slice(-5))) is """db.posts.find({}, { "comments" : { "$slice" : -5 } })"""

  db.posts.find()(_.comments(_.$slice(20, 10))) is """db.posts.find({}, { "comments" : { "$slice" : [ 20, 10 ] } })"""

  db.posts.find()(_.comments(_.$slice(-20, 10))) is """db.posts.find({}, { "comments" : { "$slice" : [ -20, 10 ] } })"""
}

