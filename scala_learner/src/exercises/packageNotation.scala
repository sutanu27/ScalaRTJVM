package exercises

object packageNotation extends App {

  class Person (val Name: String, val Movie : String,val age:Int){
    def +(aString: String) = Name+"("+aString+")"
    def unary_+ : Person = new Person(this.Name, this.Movie, this.age+1)
    def learns(asub:String): String = s"$Name wants to learn $asub"
    def learnsEnglish: String = this learns "English"
    def apply(n: Int)  = s"$Name watched $Movie $n times"
  }
val prsn=new Person("Mamata", "Rangbazz", 56)
  println(prsn+ "the painter")
  println(s"${(+prsn).Name} loves ${(+prsn).Movie} and turning to ${+prsn.age} next year")
  println(prsn learns "English")
  println(prsn.learnsEnglish)
  println(prsn(2))
}
