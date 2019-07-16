package exercises.scalafp

object FunctionalProg extends App {

  val concatenator:((String,String) => String) = new Function2[String,String,String]  {
    def apply( a : String , b: String):String = a + b
  }
  println(concatenator("sd","df"))
  val adder:((Int,Int) => Int) = new Function2[Int,Int,Int]  {
    def apply( a : Int , b: Int):Int = a + b
  }
  println(concatenator("sd","df"))
  println(adder(5,6))
}
