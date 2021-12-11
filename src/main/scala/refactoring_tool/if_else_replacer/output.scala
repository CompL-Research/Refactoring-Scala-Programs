package refactoring_tool.if_else_replacer

/**
 * The object output is concerned with outputing the refactored code and note to the user
 */
object output {


  /**
   *
   * @param code original code
   * @param ref refactored code
   * @param note based on note, different msg for different refactoring is given out.
   */
  def printRefactorings(code: scala.meta.Tree, ref: scala.meta.Tree, note: String): Unit = {

    println("Given code Snippet:")
    println(code)

    note match {
      case "" =>
        println("Refactoring Not Found.")
        println()

      case "loopPattern" =>
        println("Refactoring:")
        println(ref)
        println()

      case "pattern" =>
        println("Refactoring:")
        println(ref)
        println()

      case "not" =>
        println("Conversion to Pattern Match expression is inefficient\n")
        println()

      case _ =>


    }
  }

}
