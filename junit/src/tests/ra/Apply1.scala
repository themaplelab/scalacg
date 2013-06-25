package tests.ra

object Apply1 {

  def main(args: Array[String]) = {
    val v1 = AddFive()(1) // calls generated constructor, then calls apply()
    println(v1)
    
    val v2 = AddFive() // calls generated constructor
    v2(1) // calls apply()
    println(v2)
    
    val v3 = AddFive() 
    v3.apply(1) // calls apply()
    println(v3)
  }

  case class AddFive {
    def apply(x: Int) = x + 5
  }
  
}