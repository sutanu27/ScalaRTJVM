package exercises

object scala_oops extends App {
  val wtr=new Writer("Mamata","Bannerjee", 56)
  println(wtr.Fullname())

  val nvl=new Novel("Kothanjali",2014, wtr)
  println(nvl.AutherAge())
  println(nvl.isWriitenBy(wtr))
  val nvl2=nvl.Copy()
  println(s"This Novel is ${nvl2.name} written by ${nvl2.auther.Fullname()} published on ${nvl2.YOP}")

val count=new Counter()
  count.print
  count.incCount(3).print
  count.incCount().incCount(5).print
}


class Writer(val firstname:String, val surname: String, val year:Int){
def Fullname() : String =s"$firstname $surname"
}

class Novel(val name:String ,val YOP: Int, val auther: Writer){
  def AutherAge() : Int = auther.year-(2019-YOP)
  def isWriitenBy(auther:Writer) :Boolean = auther==this.auther
  def Copy() : Novel = new Novel(this.name, 2019, this.auther)

}

class Counter(var cnt:Int =0){
  def curCount(): Int = cnt
  def incCount (gap:Int =1)=new Counter(cnt+gap)
  def decCount (gap:Int =1) =new Counter(cnt-gap)
  def print()=println(cnt)
}