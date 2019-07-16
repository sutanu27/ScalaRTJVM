package exercises

object Collection_class extends App {
val intList:Mylist[Int]=new Cons(1,new Cons(2,new Cons(3,Empty)))
  val strList:Mylist[String]=new Cons("Two",new Cons("Pi",new Cons("Rad",Empty)))
  println(intList.ToStr)
  println(strList.ToStr)
}

abstract class Mylist[+A]{
  def head: A
  def tail: Mylist[A]
  def isEmpty : Boolean
  def add[B >: A](element: B): Mylist[B]
  def printEle : String
  def ToStr : String = "["+printEle+"]"
}

object Empty extends Mylist[Nothing]{
  def head : Nothing=throw new NoSuchElementException
  def tail : Mylist[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): Mylist[B]=new Cons(element, Empty)
  def printEle: String =""
}

class Cons[+A](h:A, mylist:Mylist[A]) extends Mylist[A]{
  def head:A=h
  def tail= mylist
  def isEmpty: Boolean = false
  def add[B >: A](element:B): Mylist[B]=new Cons(element,this)
  def printEle={
    if (mylist.isEmpty) "" +h
    else h+" "+mylist.printEle
  }
}