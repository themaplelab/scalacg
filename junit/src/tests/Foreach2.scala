package tests

/**
 * A test case that examines how methods calls within a foreach are lifted. It looks like in some cases, the body of 
 * a foreach is lifted to an anonymous apply function that is created later on in the compiler. So we don't see it 
 * at refchecks.
 */
object Foreach2 {

  def main(args: Array[String]): Unit = {
    val values = List[Int](1, 2, 3, 4) map (e => (e, e))
    for ((v, e) <- values) {
      val b = new B
      println(b(v))
    }
  }

  abstract class A {
    def apply(i: Int): String
  }
  
  class B extends A {
    def apply(i: Int) = {
      "" + i
    }
  }

}