package stomatepia

import org.scalatest.FunSuite

trait StomatepiaSuite extends FunSuite {

  class StringIs(what:String){
    def is(s:String) = test(s){ assert(what === s)}
    def isd(s:String) = test(s){
      println(s)
      println(what)
      assert(what === s)
    }
  }

  implicit def any2stringIs(any:Any) = new StringIs(any.toString)
}
