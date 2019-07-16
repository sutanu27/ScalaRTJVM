package exercises.scala2pfp

trait myset[A] extends (A => Boolean){
  override def apply(e: A): Boolean = contains(e)

  def contains(e:A): Boolean
  def +(e:A): myset[A]
  def ++(s:myset[A]): myset[A]
  def -(e:A): myset[A]
  def --(s:myset[A]): myset[A]
  def &(s:myset[A]): myset[A]
  def unary_! : myset[A]
  def map[B](f:A => B): myset[B]
  def flatmap[B](f:A => myset[B]): myset[B]
  def filter(f: A => Boolean) : myset[A]
  def foreach(f:A=> Unit):Unit
}


class emptyset[A] extends myset[A]{
  def contains(e:A): Boolean= false
  def +(e:A): myset[A] =new nonemptyset[A](e,this)
  def ++(s:myset[A]): myset[A] = s
  def -(e:A): myset[A] = this
  def --(s:myset[A]): myset[A] = this
  def &(s:myset[A]): myset[A] = this
  def unary_! : myset[A] = new roster[A](x => true)
  def map[B](f:A => B): myset[B] =new emptyset[B]
  def flatmap[B](f:A => myset[B]): myset[B] = new emptyset[B]
  def filter(f: A => Boolean) : myset[A] = this
  def foreach(f:A=> Unit):Unit =()
}

class nonemptyset[A](head:A, tail: myset[A]) extends myset[A]{
  def contains(e:A): Boolean = e==head||tail.contains(e)
  def +(e:A): myset[A]= {
    if(this.contains(e)) this
    else new nonemptyset(e,this)
  }
  def ++(s:myset[A]): myset[A]=tail ++ s + head
  def -(e:A): myset[A]= {
   if(head==e) tail
   else tail - e + head
  }
  def --(s:myset[A]): myset[A] = filter(x => !s.contains(x))
  def &(s:myset[A]): myset[A]= filter(x => s.contains(x))
  def map[B](f:A => B): myset[B] = (tail map f) + f(head)
  def flatmap[B](f:A => myset[B]): myset[B] =(tail flatmap f) ++ f(head)
  def filter(f: A => Boolean) : myset[A] ={
    val filtered = tail filter f
    if(f(head))filtered + head
    else filtered
  }
  def foreach(f:A=> Unit):Unit = {
    f(head)
    tail foreach f
  }
  def unary_! : myset[A] = new roster[A](!contains(_))
}


class roster[A](condition : A => Boolean) extends myset[A]{
  override def contains(e: A): Boolean = condition(e)
  override def +(e: A): myset[A] = new roster[A](x => condition(x)||x==e)
  override def ++(s: myset[A]): myset[A] = new roster[A](x => s(x)||condition(x))
  override def -(e: A): myset[A] = filter(_!= e)
  override def --(s: myset[A]): myset[A] = filter(!s)
  override def &(s: myset[A]): myset[A] = filter(s)
  override def map[B](f: A => B): myset[B] = throw new IllegalArgumentException("Bhison Vul")
  override def flatmap[B](f: A => myset[B]): myset[B] = throw new IllegalArgumentException("Bhison Vul")
  override def filter(f: A => Boolean): myset[A] = new roster[A](x => f(x)&&condition(x))
  override def foreach(f: A => Unit): Unit = throw new IllegalArgumentException("Bhison Vul")
  def unary_! : myset[A] = new roster[A](!condition(_))
}


object myset{
  def apply[A](values: A*): myset[A] = {
   def buildSet(valSeq: Seq[A], acc: myset[A]): myset[A]={
     if(valSeq.isEmpty) acc
     else
       buildSet(valSeq.tail, acc + valSeq.head)
   }
    buildSet(values.toSeq,new emptyset[A])
  }
}
 object  mainlike extends App{
   val s :myset[Int]= myset()
   val p = !s
   println(p.contains(2))
 }