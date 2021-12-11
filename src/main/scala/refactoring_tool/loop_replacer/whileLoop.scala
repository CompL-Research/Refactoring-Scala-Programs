package refactoring_tool.loop_replacer

import scala.meta._

object whileLoop {

  /**
   * The function identifies based on repeated and intricate pattern-matching if
   *  the loop meets the criteria to be converted into a takeWhile(h), find(x => h(x)).get, indexWhere(x=>h(x)), or indexOf(x) in scala
   * @param enumerator_body loop enumerator
   * @param loopBody
   * @return refactored tree and note
   */
  def loopBody(enumerator_body: scala.meta.Tree, loopBody: scala.meta.Tree): (scala.meta.Tree, String) = {

    var note = ""
    var ref = q"list.f()"

    enumerator_body match {
      case q"$f($list($it))" =>
        ref = q"$list.takeWhile($f)"
        note = "takeWhile"

      case q"$itx!= $listx.length && !$f($list($it))" =>
        ref = q"$list.indexWhere(x => $f(x))"
        note = "indexWhere+find"

      case q"$itx<$listx.length && !$f($list($it))" =>
        ref = q"$list.indexWhere(x => $f(x))"
        note = "indexWhere+find"

      case q"$itx!= $listx.length && $list($it) != $x" =>
        ref = q"$list.indexOf($x)"
        note = "indexOf"

      case q"$itx<$listx.length && $list($it) != $x" =>
        ref = q"$list.indexOf($x)"
        note = "indexOf"

      case _ =>
        note = ""

    }
    (ref,note)

  }


  /**
   * The function identifies based on repeated and intricate pattern-matching if
   *  the loop meets the criteria to be converted into a forAll(h), lastIndexOf(x), or lastIndexWhere(x => h(x)) in scala
   * @param enumerator_body loop enumerator
   * @param loopBody
   * @return refactored tree and note
   */
  def loopBodyConditional(enumerator_body: scala.meta.Tree, loopBody: scala.meta.Tree): (scala.meta.Tree, String) = {
    var note = ""
    var ref = q"list.f()"

    enumerator_body match {

      case Term.If(cond) =>
        cond._1 match {

          case q"!$f($list($it))" =>
            ref = q"$list.forAll($f)"
            note = "forAll"

          case q"$f($list($it))!=true" =>
            ref = q"$list.forAll($f)"
            note = "forAll"

          case q"$f($list($it))" =>
            ref = q"$list.lastIndexWhere(x => $f(x))"
            note = "lastIndexWhere"

          case  q"$x!= $list.length" =>
            ref = q"$list.lastIndexOf($x)"
            note = "lastIndexOf"

          case _ =>
            note = ""

        }

      case _=>

    }
    (ref,note)

  }

  /**
   * The loop body is analysed and based on whether a conditional is
   * present or not,
   * a different function is called to refactor in each case.
   * @param loop is a while loop node
   */
  def classifyRefactoring(loop: (scala.meta.Tree,scala.meta.Tree)): Unit = {
    val enumerator_body = loop._1
    val loop_body = loop._2
    val body = loop_body.toString()
    val enum = enumerator_body.toString()
    var conditional=false
    var original_loop = s"while($enum) $body".parse[Term].get

    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {

        case Term.If(cond) =>
         conditional=true
        case node =>
          super.apply(node)

      }
    }
    traverser(loop_body)
    if(conditional) {
      var ref = loopBodyConditional(enumerator_body,loop_body)._1
      var note = loopBodyConditional(enumerator_body,loop_body)._2
      output.printRefactorings(original_loop, ref, note)
    }

    else {
      var ref = loopBody(enumerator_body, loop_body)._1
      var note = loopBody(enumerator_body, loop_body)._2
      output.printRefactorings(original_loop, ref, note)
    }


  }

  /**
   *
   * @param loops is a list of while-loops in the format (loop-enumerator,loop-body)
   *  it calls classifyRefactoring function for each list element.
   */
  def refactor(loops: List[(scala.meta.Tree,scala.meta.Tree)]): Unit = {
    loops.foreach((f:(scala.meta.Tree,scala.meta.Tree))=>classifyRefactoring(f))
  }

}
