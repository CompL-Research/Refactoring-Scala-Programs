package refactoring_tool.testing

import scala.collection.mutable.ListBuffer

object loopRefactor2 extends App {

  //1. reduce
  def loop1(xs: List[Int]): Int = {
    var sum = 0
    for (x <- xs) {
      sum += x
    }
    sum
  }

  def ref1(xs: List[Int]): Int = {
    xs.reduce((x, y) => x + y)
  }

  //2. map
  def g(x: Int): Int = {
    x * 2 + 5
  }

  def loop2(xs: List[Int]): List[Int] = {
    var list = ListBuffer[Int]()
    for (x <- xs) {
      list += g(x) //2*x
    }
    list.toList
  }

  def ref2(xs: List[Int]): List[Int] = {
    xs.map(x => g(x)) //x * 2
  }

  //3. flatMap
  def f(x: String): String = {
    x + "abc"
  }

  def loop3(xs: List[String]): List[Char] = {
    var ans = ListBuffer[Char]()
    for (x <- xs) {
      for (t <- f(x)) {
        ans += t
      }
    }
    ans.toList
  }

  def ref3(xs: List[String]): List[Char] = {
    xs.flatMap((x: String) => f(x))
  }

  //  var xs = List(1,2,3,4)
  //  println(xs.map((x:Int)=> g(x)))
  //  println(xs.flatMap(x => List(x, x*2, x%2)))

  //4. filter
  def loop4(xs: List[Int]): List[Int] = {
    var list = ListBuffer[Int]()
    for (x <- xs) {
      if (h(x)) list += x
    }
    list.toList
  }

  def ref4(xs: List[Int]): List[Int] = {
    xs.filter(h)
  }

  //5. filterNot
  def loop5(xs: List[Int]): List[Int] = {
    var list = ListBuffer[Int]()
    for (x <- xs) {
      if (!h(x)) list += x
    }
    list.toList
  }

  def ref5(xs: List[Int]): List[Int] = {
    xs.filterNot(h)
  }

  //6. map+filter
  def loop6(xs: List[Int]): List[Int] = {
    var list = ListBuffer[Int]()
    for (x <- xs) {
      if (h(x)) list += g(x)
    }
    list.toList
  }

  def ref6(xs: List[Int]): List[Int] = {
    xs.filter(h).map((x: Int) => g(x))
  }

  //7. foreach
  def loop7(xs: List[Int]): Unit = {
    for (x <- xs) {
      println(x)
    }
  }

  def ref7(xs: List[Int]): Unit = {
    xs.foreach(println)
  }

  //8. foreach + filter
  def loop8(xs: List[Int]): Unit = {
    for (x <- xs) {
      if (h(x))
        println(x)
    }
  }

  def ref8(xs: List[Int]): Unit = {
    xs.filter(_ % 2 == 0).foreach(println)
  }

  //9. forAll
  def h(x: Int): Boolean = {
    x % 2 == 0
  }

  def loop9(xs: List[Int]): Boolean = {
    var ans = true
    var i = 0
    while (ans) {
      if (!h(xs(i)))
        ans = false
      i = i + 1
    }
    ans
  }

  def ref9(xs: List[Int]): Boolean = {
    xs.forall(h)
  }

  //10. takeWhile
  def loop10(xs: List[Int]): List[Int] = {
    var list = ListBuffer[Int]()
    var i = 0
    while (h(xs(i))) {
      list += xs(i)
      i += 1
    }
    list.toList
  }

  def ref10(xs: List[Int]): List[Int] = {
    xs.takeWhile(h)
  }

  //11. dropWhile
  //i to x -> x inclusive
  //i until x -> x excluded

  def loop11(xs: List[Int]): List[Int] = {
    var list = ListBuffer[Int]()
    var i = 0
    while (h(xs(i))) {
      i += 1
    }
    while (i != xs.length) {
      list += xs(i)
      i += 1
    }
    list.toList
  }

  def ref11(xs: List[Int]): List[Int] = {
    xs.dropWhile(h)
  }

  //12. indexOf (gives first occurance only)
  def loop12(xs: List[Int], x: Int): Int = {
    var index = -1
    while (index != xs.length && xs(index) != x) {
      index = index + 1 //index += 1
    }
    index
  }

  def ref12(xs: List[Int], x: Int): Int = {
    xs.indexOf(x)
  }

  //13. lastIndexOf
  def loop13a(xs: List[Int], x: Int): Int = {
    var index = -1
    var i = 0
    for (i <- xs.indices) {
      if (xs(i) == x)
        index = i
    }
    index
  }

  def loop13b(xs: List[Int], x: Int): Int = {
    var index = -1
    var i = 0
    while (i != xs.length) {
      if (xs(i) == x)
        index = i
      i += 1
    }
    index
  }

  def ref13(xs: List[Int], x: Int): Int = {
    xs.lastIndexOf(x)
  }

  //14. contains- check for an element
  def loop14(xs: List[Int], t: Int): Boolean = {
    var present = false
    for (x <- xs) {
      if (x == t)
        present = true
    }
    present
  }

  def ref14(xs: List[Int], x: Int): Boolean = {
    xs.contains(x)
  }

  //15. find-	first element satisfying a predicate
  def loop15(xs: List[Int]): Int = {
    var i = 0
    while (i < xs.length && !h(xs(i))) {
      i = i + 1
    }
    xs(i)
  }

  def ref15(xs: List[Int]): Int = {
    xs.find(x => h(x)).get
  }

  //16. count- no. of times an element occurs
  def loop16(xs: List[Int], t: Int): Int = {
    var count = 0
    for (x <- xs) {
      if (x == t)
        count = count + 1
    }
    count
  }

  def ref16(xs: List[Int], x: Int): Int = {
    xs.count(_ == x)
  }

  // 17. distinct - remove duplicates from xs List
  def loop17a(xs: List[Int]): List[Int] = {
    var l = ListBuffer[Int]()
    l += xs.head
    for (x <- xs) {
      if (!l.contains(x))
        l += x
    }
    l.toList
  }

  def loop17b(xs: List[Int]): List[Int] = {
    var l = ListBuffer[Int]()
    l += xs.head
    for (x <- xs) {
      var found = false
      for (t <- l) {
        if (t == x)
          found = true
      }
      if (!found)
        l += x
    }
    l.toList
  }

  def ref17(xs: List[Int]): List[Int] = {
    xs.distinct
  }

  //18. flatten
  def loop18(xs: List[String]): List[Char] = {
    var ans = ListBuffer[Char]()
    for (x <- xs) {
      for (i <- x) {
        ans += i
      }
    }
    ans.toList
  }

  def ref18(xs: List[String]): List[Char] = {
    xs.flatten
  }

  //19. indexWhere
  def loop19(xs: List[Int]): Int = {
    var index = -1
    while (index < xs.length && !h(xs(index))) {
      index = index + 1 //index += 1
    }
    index
  }

  def ref19(xs: List[Int]): Int = {
    xs.indexWhere(x => h(x))
  }

  //20. lastIndexWhere
  def loop20a(xs: List[Int]): Int = {
    var index = -1
    for (i <- xs.indices) {
      if (h(xs(i)))
        index = i
    }
    index
  }

  def loop20b(xs: List[Int]): Int = {
    var index = -1
    var i = 0
    while (i < xs.length) {
      if (h(xs(i)))
        index = i
      i += 1
    }
    index
  }

  def ref20(xs: List[Int]): Int = {
    xs.lastIndexWhere(x => h(x))
  }

  def func(i: List[Int]): Unit = {
    println(i)
  }

  List(1, 2, 3, 4, 5, 6).sliding(2).foreach((x) => func(x))


}
