/**
 * The package loop_replacer contains 4 singleton objects with
 * various functions to convert a number of loops statement to
 * to higher-order functions or in-built methods of Scala.
 */
package refactoring_tool.loop_replacer
import scala.meta.{Defn, Enumerator, Term, Traverser, Tree}

/**
 * The object has functions devoted to segregating
 * the meta codes for appropriate refactoring.
 */
object checkLoop extends App {

  /**
   * @param program the scalameta tree passed down
   *                from getFromFile/getFromTerminal
   *  The function checks is there are two while loops, and
   *  passes the loops to the refactor methos of the multiLoop object.
   */
  def isMultiLoop(program: scala.meta.Tree): Unit = {
    var multiLoops = scala.collection.mutable.ListBuffer[(Term, Term)]()
    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Term.While(loop) =>
          multiLoops += loop

        case node =>
          super.apply(node)
      }
    }
    traverser(program)
    if(multiLoops.size>1)
      multiLoop.refactor(multiLoops.toList)
  }

  /**
   *
   * @param program the scalameta tree passed down
   *                from getFromFile/getFromTerminal
   *  Checks for 'for' loops and puts them in a list,
   *  likewise for 'while' loops and passed each of the
   *  lists to the refactor function of corresponding object.
   *  Also in case of a definition node, 2 or more while loops
   *  might lead to multiloop refactoring.
   */
  def classifyLoop(program: scala.meta.Tree): Unit = {
    var forLoops = scala.collection.mutable.ListBuffer[(List[Enumerator], Term)]()
    var whileLoops = scala.collection.mutable.ListBuffer[(Term, Term)]()
    val traverser1 = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Term.For(loop)  =>
          forLoops += loop

        case Term.While(loop) =>
          whileLoops += loop

        case node =>
          super.apply(node)

      }
    }

    val traverser2 = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Defn.Def(defn) =>
          isMultiLoop(defn._6)

        case node =>
          super.apply(node)

      }
    }
    traverser1(program)
    traverser2(program)

    if(forLoops.nonEmpty)
      forLoop.refactor(forLoops.toList)
    if(whileLoops.nonEmpty)
      whileLoop.refactor(whileLoops.toList)
  }

  /**
   *
   * @param program the scalameta tree that will be analysed.
   * if a single loop is found, classifyLoop will be called, for exact
   * estimate for refactoring.
   */
  def isLoop(program: scala.meta.Tree): Unit = {
    var loopPresent=false

    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Term.For(loop)  =>
          loopPresent = true

        case Term.While(loop) =>
          loopPresent = true

        case node =>
          super.apply(node)

      }
    }
    traverser(program)
    if(loopPresent)
      classifyLoop(program)


  }


}
