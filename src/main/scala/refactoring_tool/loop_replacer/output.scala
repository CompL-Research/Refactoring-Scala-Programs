package refactoring_tool.loop_replacer

/**
 * The object output is concerned with outputing the refactored code and note to the user
 */
object output {

  /**
   *
   * @param loop original code
   * @param ref refactored code
   * @param note based on note, different msg for different refactoring is given out.
   */
  def printRefactorings(loop: scala.meta.Tree, ref: scala.meta.Tree, note: String): Unit = {

    println("Given code Snippet:")
    println(loop)

    note match {
      case "" =>
        println("Refactoring Not Found.")
        println()

      case "mapS" =>
        println("Likely Sequence Method Replacement:")
        println("map")
        println()

      case "filterS" =>
        println("Likely Sequence Method Replacement:")
        println("filter")
        println()

      case "flatMapS" =>
        println("Likely Sequence Method Replacement:")
        println("flatMap")
        println()

      case "filter+mapS" =>
        println("Likely Sequence Method Replacement:")
        println("filter + map")
        println()

      case "flattenS" =>
        println("Likely Sequence Method Replacement:")
        println("flatten")
        println()

      case "distinctS" =>
        println("Likely Sequence Method Replacement:")
        println("distinct")
        println()

      case "indexWhereS" =>
        println("Likely Sequence Method Replacement:")
        println("indexWhere")
        println()

      case "lastIndexWhereS" =>
        println("Likely Sequence Method Replacement:")
        println("lastIndexWhere")
        println()

      case "indexWhere+find" =>
        println("Refactoring:")
        println(ref)
        println("Other Possible Sequence Methods: indexWhere, find")
        println()

      case _ =>
        println("Refactoring:")
        println(ref)
        println()

    }
  }
}
