package refactoring_tool.testing

import scala.collection.mutable.ListBuffer

object ifelseRefactor2 {

  //test example
  val enteredPassword = 10

  def testIf1(password: Int) = {
    if (password == enteredPassword) {
      println(s"User is authenticated.")
    }
    else {
      println(s"Entered password is invalid.")
    }
  }

  //test example
  def testIf2(a: Int) = {
    var result = if (a == 100) {
      val d = 1
      println("100")
    }
    else if (a == 200) {
      val d = 2
      println("2000")
    }
    else {
      val d = 3
      println("none")
    }
  }


  //test example
  def g(a: Int) = ???

  def f(a: Int): Int = {
    a + 1
  }

  def h(a: Int, b: Int): Unit = {
    a + b
  }

  def testIf3(a: Int, b: Int, c: Int) = {
    var result = if (a == 0) g(a)
    else if (b == 4) g(b)
    else if (b == 10 && a == 5) f(a + b)
    else if (b == 5 && a < 20) h(a, c)
    else a + b + c
  }


  //test example
  def testIf4(a: Int, d: Int, c: Int) = {
    var result = if (a > c) 0
    else if (a < c && a > d) 1
    else if (a == d) 2
    else if (a < d) 3
    else if (a == 500) 4
    else 5
  }


  //test example
  var (a, b, c) = (110, 200, 200)
  if (a == 100) println("aaa")
  else if (a == 110 || b == 110) println("eee")
  else if (b == 200 && c < a) println("bbb")
  else if (b == 200 && c == 200) println("ddd")
  else println("ccc")

  //test example

  def test(xs: List[Int]): List[Int] = {
    var result = ListBuffer[Int]()
    for (x <- xs) {
      if (x == 1) result += f(x + 1)
      else if (x == 2) result += f(x + 2)
      else if (x == 3) result += f(x + 3)
      else result += f(x + 4)
    }
    result.toList
  }


}
