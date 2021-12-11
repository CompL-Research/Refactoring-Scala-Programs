/**
 * The package if_else_replacer contains 4 singleton objects with
 * various functions to convert a nested if-else statement to
 * a pattern match expression.
 */
package refactoring_tool.if_else_replacer

import scala.meta._

object checkConditonal {

  /**
   *  If a for-loop is found it can be a combined pattern-match
   *  else in case of an if-node, pattern-matching.
   * @param program the scalameta tree
   */

  def isConditional(program: scala.meta.Tree): Unit = {
    var ifMap = scala.collection.mutable.LinkedHashMap.empty[String, String]
    var bool1=false
    var bool2=false
    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {

        case Term.While(loop)=>

        case Term.If(cond) =>
          bool1=true
          patternMatch.classifyRefactoring(cond)

        case Term.For(loop) =>
          bool2=true
          combinedPatternMatch.classifyRefactoring(loop)

        case node =>
          super.apply(node)

      }
    }
    traverser(program)

  }

}
