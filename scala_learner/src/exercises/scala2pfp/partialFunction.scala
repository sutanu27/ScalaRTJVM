package exercises.scala2pfp

object partialFunction extends App{

  val partial=new PartialFunction[Int,Int]{
    override def apply(x:Int):Int={
      x match{
        case 2 =>20
        case 3 => 30
        case 4 => 40
      }
    }

    override def isDefinedAt(x: Int): Boolean = x==2||x==3||x==4
  }

  println(partial.isDefinedAt(2))

  val chatbot:PartialFunction[String,String]= {
    case "Hey" => "Hello!"
    case "I am an Indian" => "Bitch Lasanga"
    case "Dumb ot what?" => "I India You loose"
  }

   scala.io.Source.stdin.getLines().foreach(x => println("random indian:"+chatbot(x)))

}
