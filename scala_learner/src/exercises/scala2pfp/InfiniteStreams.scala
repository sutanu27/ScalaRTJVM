package exercises.scala2pfp

import java.util.NoSuchElementException

object InfiniteStreams extends App {
val POS=mystream.from(1)(_+1)
def FIB(f:Int =1, s:Int =1):mystream[Int]=new Cons(f,FIB(f + s))
 // POS.take(100).foreach(println)
  //println(POS.take(10).toList())

    def Prime(stream:mystream[Int]): mystream[Int]={
      if(stream.isEmpty) stream
      else new Cons(stream.head,Prime(stream.tail.filter(_%stream.head!=0)))
    }
println(Prime(mystream.from(2)(_+1)).take(1000).toList())

}


trait mystream[+A]{
  def isEmpty : Boolean
  def head: A
  def tail : mystream[A]
  def #::[B >: A](e:B): mystream[B]
  def ++[B >: A](otherStream: =>mystream[B]): mystream[B]
  def foreach(f:A=>Unit): Unit
  def map[B >:A](f:A=>B):mystream[B]
  def flatmap[B >: A](f:A=> mystream[B]): mystream[B]
  def filter(f:A=>Boolean): mystream[A]
  def take(n:Int): mystream[A]
  final def  toList[B >: A](acc: List[B]=Nil): List[B]={
    if(isEmpty) {
      acc.reverse
    }
    else {
      tail.toList(head :: acc)
    }
  }
}

object emptystream extends mystream[Nothing]{
  def isEmpty : Boolean = true
  def head: Nothing =throw new NoSuchElementException
  def tail : mystream[Nothing] = throw new NoSuchElementException
  def #::[B >: Nothing](e:B): mystream[B] = new Cons(e,this)
  def ++[B >: Nothing](otherStream: => mystream[B]): mystream[B] =otherStream
  def foreach(f:Nothing=>Unit): Unit = ()
  def map[B >:Nothing](f:Nothing=>B):mystream[B]=this
  def flatmap[B >: Nothing](f:Nothing=> mystream[B]): mystream[B]=this
  def filter(f:Nothing=>Boolean): mystream[Nothing]=this
  def take(n:Int): mystream[Nothing]=this
}

class Cons[+A](hd: A, tl: => mystream[A]) extends mystream[A]{
  def isEmpty : Boolean= false
  override val head: A = hd
  override lazy val tail : mystream[A] = tl
  def #::[B >: A](e:B): mystream[B]= new Cons(e,this)
  def ++[B >: A](otherStream: => mystream[B]): mystream[B]= new Cons(head,tail ++ otherStream)
  def foreach(f:A=>Unit): Unit ={
    f(head)
    tail.foreach(f)
  }
  def map[B >:A](f:A=>B):mystream[B] =new Cons(f(head), tail.map(f))
  def flatmap[B >: A](f:A=> mystream[B]): mystream[B] = f(head) ++ tail.flatmap(f)
  def filter(f:A=>Boolean): mystream[A] ={
    if(f(head)) new Cons(head, tail.filter(f))
    else tail.filter(f)
  }
  def take(n:Int): mystream[A] = {
    if(n==0) emptystream
    else if(n==1) new Cons(head,emptystream)
    else new Cons(head,tail.take(n-1))
  }
  def takeAsList(n:Int):List[A]=take(n).toList()



}

object mystream{
  def from[A](start:A)(gen:A=>A) : mystream[A]=new Cons(start,mystream.from(gen(start))(gen))
}
