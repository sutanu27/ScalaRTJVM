package exercises

object Annonymous_class extends App{
abstract class Animal{
  def eat: String
}

  val atrAnimal : Animal = new Animal{
    override def eat: String  ="galpgalp"
  }
  println(atrAnimal)
  println(atrAnimal.eat)

}
