package stomatepia

import org.scalatest.FunSuite

class DotNotation extends FunSuite {

  implicit def isCursor(c:Cursor) = new {
    def is(s:String) = test(s){ assert(c.toString === s) }
  }

  object Foo extends Stomatepia {
    case class Foo(name:String) extends Embedded[Foo](name){
      val shape = string("shape")
      val color = string("color")
      val thick = boolean("thick")
    }
    val foo = array(Foo("foo"))
  }

  object Person extends Stomatepia {
    val name = string("name")
    val address = Address("address")
    val likes = array(string("likes"))

    case class Address(name:String) extends Embedded[Address](name){
      val city = string("city")
      val state = string("state")
    }
  }

  object Blogpost extends Stomatepia {
    val title = string("title")
    val author = Author("author")
    val comments = array(Comments("comments"))

    case class Author(_name:String) extends Embedded[Author](_name){
      val id = int("id")
      val name = string("name")
    }

    case class Comments(name:String) extends Embedded[Comments](name){
      val by = string("by")
      val text = string("text")
    }
  }

  object db {
    val foo = Foo.Collection("foo")
    val persons = Person.Collection("persons")
    val blogposts = Blogpost.Collection("blogposts")
    val blog = Blogpost.Collection("blog")
  }

//  test("intro"){
    db.persons.find(_.name("Joe"))          is """db.persons.find({ "name" : 'Joe' })"""

    db.persons.find(_.address.state("CA"))  is """db.persons.find({ "address.state" : 'CA' })"""

    db.persons.find(_.likes("math"))        is """db.persons.find({ "likes" : 'math' })"""

    db.blogposts.find(_.comments.by("Ada")) is """db.blogposts.find({ "comments.by" : 'Ada' })"""

    db.persons.ensureIndex(_.address.state)

    db.blogposts.ensureIndex(_.comments.by)
//  }

//  test("dot notation vs subobjects"){
    db.blog.save(_.title("My First Post"), _.author(_.name("Jane"), _.id(1)))

    db.blog.findOne(_.author.name("Jane"))             is """db.blog.findOne({ "author.name" : 'Jane' })"""

    db.blog.findOne(_.author(_.name("Jane"), _.id(1))) is """db.blog.findOne({ "author" : { "name" : 'Jane', "id" : 1 } })"""

    db.blog.findOne(_.author(_.name("Jane")))          is """db.blog.findOne({ "author" : { "name" : 'Jane' } })"""

    db.blog.findOne(_.author(_.id(1), _.name("Jane"))) is """db.blog.findOne({ "author" : { "id" : 1, "name" : 'Jane' } })"""
//  }

//  test("Array Element by Position"){
    db.blogposts.find(_.comments(0).by("Abe")) is """db.blogposts.find({ "comments.0.by" : 'Abe' })"""
//  }

//  test("Matching with $elemMatch"){
    db.foo.find(_.foo.shape("square"), _.foo.color("purple"))           is """db.foo.find({ "foo.shape" : 'square', "foo.color" : 'purple' })"""

    db.foo.find(_.foo(_.shape("square"), _.color("purple")))            is """db.foo.find({ "foo" : { "shape" : 'square', "color" : 'purple' } })"""

    db.foo.find(_.foo(_.$elemMatch(_.shape("square"), _.color("purple")))) is """db.foo.find({ "foo" : { "$elemMatch" : { "shape" : 'square', "color" : 'purple' } } })"""
//  }
}


