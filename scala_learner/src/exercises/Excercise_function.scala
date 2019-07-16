package exercises

object Excercise_function extends App {
  def Greeting(name: String, age: Int): String = {
    "I am " + name + "(" + age + ")"
  }

  println(Greeting("Sutanu", 20))


  def Factorial(num: Int): Int = {
    if (num == 0) 1
    else if (num == 1) 1
    else num * Factorial(num - 1)
  }

  println(Factorial(5))


  def Fibonacci(num : Int): Int = {
  if(num==1) 1
  else if(num==2) 1
  else Fibonacci(num-1)+Fibonacci(num-2)
  }

  println(Fibonacci(6))

  def isPrime(num: Int): Boolean ={
    def checking(ita: Int) : Boolean ={
      if(ita<=1) true
      else num % ita != 0 && checking(ita - 1)
  }
     checking(num/2)
  }
  println(isPrime(3))
}