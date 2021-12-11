import scala.collection.mutable.ListBuffer

object MainObject{
  def main(args:Array[String]){
    print("Hello Scala")

    var (a,b,c,d) = (0,0,0,0)
    var ys = List("s1","ss","s3")
    var ans =ListBuffer[Char]()
    var xs = List(1,3,4,5,6,7)
    var x = 5
    var index= -1
    var i=0
    var result = ListBuffer[Int]()


    if(a==100) println("aaa")
    else if(a==110 || b==110) println("eee")
    else if(b==200 && c<a) println("bbb")
    else if(b==200 && c==200) println("ddd")
    else println("ccc")


    if(a==100){
      val d =1
      println("100")
    }
    else if(a==200){
      val d=2
      println("2000")
    }
    else {
      val d=3
      println("none")
    }

    //
    for (x <- xs){
      if(x==1) result += f(x+1)
      else if(x==2) result += f(x+2)
      else if(x==3) result += f(x+3)
      else result += f(x+4)
    }

    if(a>c) 0
    else if (a<c && a>d) 1
    else if (a==d) 2
    else if (a<d) 3
    else if (a==500) 4
    else 5

//    for (x <- xs) {
//      if(h(x))
//        println(x)
//    }
//
//    for(i <- xs.indices){
//      if(xs(i)==x)
//        index=i
//    }
//
//
//    def h(x: Int):Boolean={
//      x%2==0
//    }
//
//    def loop11(xs: List[Int]):List[Int] ={
//      var list = ListBuffer[Int]()
//      var i =0
//      while (h(xs(i))) {
//        i += 1
//      }
//      while(i!=xs.length){
//        list += xs(i)
//        i += 1
//      }
//      list.toList
//    }
//
    def f(x:Int):Int = {
      x+3
    }
//
//    for(y <- ys){
//      for(e <- y){
//        ans += e
//      }
//    }
//
//    while(index<xs.length && !h(xs(index))){
//      index = index+1
//    }
//



  }
}
