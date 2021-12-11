package refactoring_tool.if_else_replacer

import scala.meta._

object combinedPatternMatch {

  /**
   * Analyse the if-map with a series of pattern-matching and return the
   * altered map with augmentations to directly fit a pattern-match scala body.
   * @param ifMap
   * @param iterator
   * @return sol - a boolean, iterator, newlist and altered if-map.
   */
  def predicateAnalysis(ifMap: scala.collection.mutable.LinkedHashMap[String, String], iterator: String): (Boolean, String, String, scala.collection.mutable.LinkedHashMap[String, String]) = {
    var enumerator = ""
    var newList = "" //result
    var pattern = true
    var ifMap2 = scala.collection.mutable.LinkedHashMap.empty[String, String]
    if(ifMap.size<3 || ifMap.last._1!="else") {
      pattern=false
      (pattern,"","",ifMap)
    }
    else{

      for ((pred, stat) <- ifMap) {
        if (pred != "else" && pattern==true) {
          s"$pred".parse[Term].get match {
            case q"$x==$y" =>
              q"$x" match {
                case Term.Name(x) =>
                  if(x!=iterator) pattern=false
                  else{
                    enumerator = x
                    if(newList.isEmpty){
                      newList = stat.substring(0,stat.indexOf("+=")).replaceAll("\\s", "")
                    }
                    else{
                      if(newList!=stat.substring(0,stat.indexOf("+=")).replaceAll("\\s", "")) pattern=false
                    }
                    q"$y" match {
                      case Term.Name(y) =>
                        ifMap2 += ("`" + s"$y" + "`" -> stat.substring(stat.indexOf("+=")+2,stat.length))
                      case Lit.Int(value) =>
                        ifMap2 += (value.toString -> stat.substring(stat.indexOf("+=")+2,stat.length))
                      case _ =>
                        pattern=false
                    }
                  }
                case _ => pattern=false
              }
            case _ => pattern=false
          }
        }
        else {
          if(newList.isEmpty){
            pattern=false
          }
          else{
            if(newList!=stat.substring(0,stat.indexOf("+=")).replaceAll("\\s", "")) pattern=false
          }
          ifMap2 += ("_" -> stat.substring(stat.indexOf("+=")+2,stat.length))
        }

      }
      (pattern, enumerator, newList, ifMap2)
    }


  }

  /**
   * Build an if-map and do analysis on the map with the iterator.
   * If the analysis yields a solution, pass the refactored code else a
   * garbage code with note.
   * @param program loop body
   * @param iterator loop iterator (x)
   * @param original_list list (xs)
   * @return a refactored code tree and a note
   */
  def loopRefactor(program: scala.meta.Tree, iterator: String, original_list: String): (scala.meta.Tree, String) = {
    var ifMap = scala.collection.mutable.LinkedHashMap.empty[String, String]
    var note = ""
    val traverser: meta.Traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Term.If(if_statement) =>
          var if_predicate = if_statement._1.toString()
          var if_body = if_statement._2.toString()
          var else_body = if_statement._3
          ifMap += (if_predicate -> if_body)
          var loop = true
          while (loop) {
            else_body match {
              case Term.If(node) =>
                var pred = node._1.toString()
                var stat = node._2.toString()
                ifMap += (pred -> stat)
                else_body = node._3
              case _ =>
                loop = false
            }
          }
          ifMap += ("else" -> else_body.toString())
        case node =>
          super.apply(node)
      }
    }
    traverser(program)
    var pattern = ""
    var sol = predicateAnalysis(ifMap, iterator)
    if(!sol._1) {
      (q"list.f()", note)
    }
    else{
      val newList = sol._3
      ifMap = sol._4
      pattern += "var " + newList + " = " + original_list + ".map{\n" + " " + iterator + " => " + iterator + " match {\n"
      for((pred->stat) <- ifMap){
        pattern += " case " + s"$pred" + " => " + s"$stat" + "\n"
      }
      pattern += " }\n" + "}\n"
      var tree = s"$pattern".parse[Stat].get
      note = "loopPattern"
      (tree, note)
    }

  }

  /**
   * Pass the loop iterator, loop body and the list over which loop was running, to the refactor function.
   * @param loop A loop which may contain an if-else body.
   */

  def classifyRefactoring(loop: (List[Enumerator],scala.meta.Tree)): Unit = {

    var enumerator = loop._1.head.toString() // x <- xs
    val iterator = enumerator.substring(0,enumerator.indexOf(" ")) //x
    val original_list = enumerator.substring(enumerator.indexOf("<-")+"<- ".length, enumerator.length) //xs
    val loop_body = loop._2.toString()
    var Loop = s"for($iterator <- $original_list) $loop_body".parse[Term].get
    var ref = loopRefactor(loop._2, iterator, original_list)._1
    var note = loopRefactor(loop._2, iterator, original_list)._2
    if(note=="loopPattern")
      output.printRefactorings(Loop,ref,note)

  }
}
