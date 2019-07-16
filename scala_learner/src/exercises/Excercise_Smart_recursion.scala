package exercises

object Excercise_Smart_recursion extends App {

  def RepeatedString(num: Int, str: String): String = {
    def tailRepeatedString(num: Int, str: String, Result: String): String = {
      if (num == 1) Result
      else tailRepeatedString(num - 1,str, Result + str)
    }

    tailRepeatedString(num, str, str)
  }
  println(RepeatedString(10,"Sutanu"))


  def isPrime(num:Int) : Boolean ={
    def isPrimeSmart(itra:Int ,num:Int , status: Boolean): Boolean={
      if(itra==1) true
      else {
        if (num % itra == 0) false
        else {
          isPrimeSmart(itra - 1, num, true)
        }
      }
    }
    if(num<=1) false
    else if(num==2) true
    else isPrimeSmart(num/2,num,true)
  }
  println(isPrime(96))



  def Fibonacci(num : Int): Int = {
    def samrtFibonacci( num1: Int, num2:Int, itr:Int): Int = {
      if (num == itr) num1+num2
      else {
        samrtFibonacci(num2,num1+num2,itr+1)
      }
    }
    if (num==1) 1
    else if(num==2) 1
    else samrtFibonacci(1,1,3)
  }
  println(Fibonacci(7))
}

