package exercises

object Generic_oops_ex extends App {
  val list : Mylist1[Int]=new Cons1(1,new Cons1(2,new Cons1(3,Empty1)))
  println(list.ToStr)
  println(list.filter(_%2==0).ToStr)
  println(list.map(_*2).ToStr)
  println(list.flatMap(x=> new Cons1(x,new Cons1(x+1,Empty1))
  ).ToStr)
  list.forEach(println)
}

abstract class Mylist1[+A]{
  def head: A
  def tail: Mylist1[A]
  def isEmpty : Boolean
  def add[B >: A](element: B): Mylist1[B]
  def printEle : String
  def ToStr : String = "["+printEle+"]"
  def map[B](trans:A => B) : Mylist1[B]
  def filter(pred:A => Boolean) : Mylist1[A]
  def flatMap[B](trans:A => Mylist1[B]): Mylist1[B]
  def ++[B >:A](list: Mylist1[B]) : Mylist1[B]
  def forEach(f: A => Unit): Unit
}

object Empty1 extends Mylist1[Nothing]{
  def head : Nothing=throw new NoSuchElementException
  def tail : Mylist1[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): Mylist1[B]=new Cons1(element, Empty1)
  def printEle: String =""
  def map[B](trans:Nothing => B) : Mylist1[B]=Empty1
  def filter(pred:Nothing => Boolean) : Mylist1[Nothing]=Empty1
  def ++[B >: Nothing](list: Mylist1[B]) : Mylist1[B]=list
  def flatMap[B](trans:Nothing => Mylist1[B]): Mylist1[B]=Empty1
  def forEach(f: Nothing => Unit) :Unit= ()
}

class Cons1[+A](h:A, mylist:Mylist1[A]) extends Mylist1[A]{
  def head:A=h
  def tail= mylist
  def isEmpty: Boolean = false
  def add[B >: A](element:B): Mylist1[B]=new Cons1(element,this)
  def printEle={
    if (mylist.isEmpty) "" +h
    else h+" "+mylist.printEle
  }

  def map[B](trans:A => B) : Mylist1[B]={
  new Cons1(trans(h),mylist.map(trans))
  }

  def filter(pred: A => Boolean): Mylist1[A] = {
    if (pred(h)) {
      new Cons1(h, mylist.filter(pred))
    }
    else mylist.filter(pred)
  }
  def ++[B >: A](list: Mylist1[B]) : Mylist1[B]= {
    new Cons1(h,mylist ++ list)
  }
  def flatMap[B](trans:A => Mylist1[B]): Mylist1[B]={
   trans(h) ++ tail.flatMap(trans)
  }

  def forEach(f: A => Unit): Unit ={
    f(h)
    tail.forEach(f)
  }
}

