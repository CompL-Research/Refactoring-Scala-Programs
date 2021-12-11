package refactoring_tool.loop_replacer
import scala.meta._

object multiLoop {

  /**
   * the 2 while loop tree nodes are analysed together to find if they can be refactored by the dropwhile scala function.
   * @param list each list element is a pair of scalameta trees- the pair is a while-loop tree node
   */
  def classifyRefactoring(list: List[(scala.meta.Tree,scala.meta.Tree)]): Unit = {
    var loop1_enumerator = list.head._1
    val enum1 = loop1_enumerator.toString()
    var loop1_body = list(1)._2
    val body1 = loop1_body.toString()
    var loop2_enumerator = list(1)._1
    val enum2 = loop2_enumerator.toString()
    var loop2_body = list(1)._2
    val body2 = loop2_body.toString()

    val Loops = s"while($enum1) $body1" + s"while ($enum2) $body2"
//    val originalLoops = Loops.parse[Term].get

    var found = false
    var ref = q"list.f()"
    var note = ""
    var x: Term = Term.Name("s")

    loop1_enumerator match {
      case q"$f($list($it))" =>
        found = true
        x = f
      case _ =>

    }
    if(found){
      loop2_enumerator match {
        case q"$it != $list.length" =>
          ref = q"$list.dropWhile($x)"
          note = "dropWhile"

        case _ =>
      }

    }
    if(note!="")
      {
        println("Given code Snippet:")
        println(Loops)
        println("Refactoring:")
        println(ref)
        println()
      }

  }

  /**
   *
   * @param list is a list of for-loops in the format (loop-enumerator,loop-body)
   *  it calls classifyRefactoring function for 2 list elements at a time.
   */
  def refactor(list: List[(scala.meta.Tree,scala.meta.Tree)]): Unit = {
    list.sliding(2).foreach(x => classifyRefactoring(x))

  }

}
