package refactoring_tool.if_else_replacer

import scala.meta._

object patternMatch {
  /**
   * modify case clause to directly fit the in the pattern-match expression
   * @param elems tuple-elements.
   * @param clause the template
   * @param x term1
   * @param y term2
   * @return modified clause
   */
  def modifyCaseClause(elems: Seq[String], clause: String, x: String, y: String): String = {
    val t = elems.indexOf(x)
    var modClause = ""
    if(t<0) modClause = clause
    else {
      clause.count(_==',') match {
        case 0 => modClause = y
        case 1 =>
          t match {
            case 0 => modClause = "(" + y + clause.substring(2, clause.length)
            case 1 =>  modClause = clause.substring(0, clause.indexOf(',')+1) + y + ")"
          }
        case 2 =>
          t match {
            case 0 => modClause = "(" + y + clause.substring(2, clause.length)
            case 1 => modClause = clause.substring(0, clause.indexOf(',')+1) + y +  clause.substring(clause.indexOf(',')+2,clause.length)
            case 2 => modClause = clause.substring(0,clause.length-2) +y + ")"
          }
        case _ =>
      }
    }
    modClause
  }

  /**
   * the template for match expression is
   * returned based on the size of argument.
   * @param elems a sequence of string
   * @return the template for match expression
   */
  def getCaseClause(elems: Seq[String]) : String = {
    elems.size match {
      case 1 => "_"
      case 2 => "(_,_)"
      case 3 => "(_,_,_)"
      case _ => ""
    }

  }

  /**
   * analyse the ifMap to come up with the match expression elements set and return it.
   * @param ifMap
   * @return tuple set
   */
  def tupleSet(ifMap:  scala.collection.mutable.LinkedHashMap[String, String]) : scala.collection.mutable.Set[String] = {
    var tupleElements = scala.collection.mutable.Set.empty[String]
    for ((pred, stat) <- ifMap) {
      if (pred != "else") {
        s"$pred".parse[Term].get match {

          case q"$x==$y" =>
            q"$x" match {
              case Term.Name(x) =>
                q"$y" match {
                  case Term.Name(y) =>
                    tupleElements += s"$x"
                  case Lit.Int(value) =>
                    tupleElements += s"$x"

                  case _ =>

                }
              case _ =>

            }

          case q"$x1 && $y1" =>
            q"$x1" match {
              case q"$a==$b" =>
                q"$a" match {
                  case Term.Name(a) =>
                    q"$b" match {
                      case Term.Name(b) =>
                        tupleElements += s"$a"
                      case  Lit.Int(value) =>
                        tupleElements += s"$a"
                      case _ =>

                    }
                  case _ =>

                }
              case _ =>

            }
            q"$y1" match {
              case q"$a==$b" =>
                q"$a" match {
                  case Term.Name(a) =>
                    q"$b" match {
                      case Term.Name(b) =>
                        tupleElements += s"$a"
                      case Lit.Int(value) =>
                        tupleElements += s"$a"
                      case _ =>

                    }
                  case _ =>

                }
              case _ =>
            }

          case q"$x11 || $y11" =>
            q"$x11" match {
              case q"$a==$b" =>
                q"$a" match {
                  case Term.Name(a) =>
                    q"$b" match {
                      case Term.Name(b) =>
                        tupleElements += s"$a"
                      case Lit.Int(value) =>
                        tupleElements += s"$a"
                      case _ =>

                    }
                  case _ =>

                }
              case _ =>

            }
            q"$y11" match {
              case q"$a==$b" =>
                q"$a" match {
                  case Term.Name(a) =>
                    q"$b" match {
                      case Term.Name(b) =>
                        tupleElements += s"$a"
                      case Lit.Int(value) =>
                        tupleElements += s"$a"
                      case _ =>

                    }
                  case _ =>

                }
              case _ =>
            }

          case _ =>

        }

      }
    }
    tupleElements
  }

  /**
   * use the analysis of ifMap to identify the structure of every match pattern sub-expression
   * and use the tupleElements to structure the match expression. Together, compose and reutn
   * the final pattern-match expression.
   * @param ifMap
   * @param tupleElements
   * @return refactored code tree
   */
  def patternMatchExpression(ifMap: scala.collection.mutable.LinkedHashMap[String, String], tupleElements: scala.collection.mutable.Set[String]): scala.meta.Tree = {
    var pattern = ""
    val tupleSize = tupleElements.size
    val elems = tupleElements.toSeq
    var matchExpr = ""
    tupleSize match {
      case 1 =>
        matchExpr = elems.head
      case 2 =>
        matchExpr = "(" + elems.head + ", " + elems(1) + ")"
      case 3 =>
        matchExpr = "(" + elems.head + ", " + elems(1) + ", " + elems(2) + ")"
    }
    pattern += matchExpr + " match " + "{\n"
    for ((pred, stat) <- ifMap) {
      var clause = getCaseClause(elems)
      if (pred != "else") {
        s"$pred".parse[Term].get match {

          case q"$x==$y" =>
            q"$x" match {
              case Term.Name(x) =>
                q"$y" match {
                  case Term.Name(y) =>
                    var ny  = "`" + y + "`"
                    clause = modifyCaseClause(elems, clause, x, ny)
                    pattern += "case " + clause + " => " + s"$stat" + "\n"
                  case Lit.Int(value) =>
                    clause = modifyCaseClause(elems, clause, x, value.toString)
                    pattern += "case " + clause + " => " + s"$stat" + "\n"
                  case _ =>
                    pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                }
              case _ =>
                pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"

            }

          case q"$x1 && $y1" =>
            var found = false
            q"$x1" match {
              case q"$a==$b" =>
                q"$a" match {
                  case Term.Name(a) =>
                    q"$b" match {
                      case Term.Name(b) =>
                        found = true
                        var nb  = "`" + b + "`"
                        clause = modifyCaseClause(elems, clause, a, nb)
                      case Lit.Int(value) =>
                        found = true
                        clause = modifyCaseClause(elems, clause, a, value.toString)
                      case _ =>
                    }
                  case _ =>
                }
              case _ =>

            }
            if(found){
              q"$y1" match {
                case q"$a==$b" =>
                  q"$a" match {
                    case Term.Name(a) =>
                      q"$b" match {
                        case Term.Name(b) =>
                          var nb  = "`" + b + "`"
                          clause = modifyCaseClause(elems, clause, a, nb)
                          pattern += "case " + clause + " => " + s"$stat" + "\n"
                        case Lit.Int(value) =>
                          clause = modifyCaseClause(elems, clause, a, value.toString)
                          pattern += "case " + clause + " => " + s"$stat" + "\n"
                        case _ =>
                          pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                      }
                    case _ =>
                      pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                  }
                case _ =>
                  pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
              }
            }
            else
              pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"

          case q"$x11 || $y11" =>
            var found = true

            q"$x11" match {
              case q"$a==$b" =>
                q"$a" match {
                  case Term.Name(a) =>
                    q"$b" match {
                      case Term.Name(b) =>
                        q"$y11" match {
                          case q"$a==$b" =>
                            q"$a" match {
                              case Term.Name(a) =>
                                q"$b" match {
                                  case Term.Name(b) =>
                                    found = true
                                    var nb  = "`" + b + "`"
                                    clause = modifyCaseClause(elems, getCaseClause(elems), a, nb)
                                    pattern += "case " + clause + " => " + s"$stat" + "\n"
                                  case Lit.Int(value) =>
                                    found = true
                                    clause = modifyCaseClause(elems, getCaseClause(elems), a, value.toString)
                                    pattern += "case " + clause + " => " + s"$stat" + "\n"
                                  case _ =>
                                    pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                                }
                              case _ =>
                                pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                            }
                          case _ =>
                            pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                        }
                        if(found){
                          var nb  = "`" + b + "`"
                          clause = modifyCaseClause(elems, getCaseClause(elems), a, nb)
                          pattern += "case " + clause + " => " + s"$stat" + "\n"
                        }

                      case Lit.Int(value) =>
                        q"$y11" match {
                          case q"$a==$b" =>
                            q"$a" match {
                              case Term.Name(a) =>
                                q"$b" match {
                                  case Term.Name(b) =>
                                    found = true
                                    var nb  = "`" + b + "`"
                                    clause = modifyCaseClause(elems, getCaseClause(elems), a, nb)
                                    pattern += "case " + clause + " => " + s"$stat" + "\n"
                                  case Lit.Int(value) =>
                                    found = true
                                    clause = modifyCaseClause(elems, getCaseClause(elems), a, value.toString)
                                    pattern += "case " + clause + " => " + s"$stat" + "\n"
                                  case _ =>
                                    pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                                }
                              case _ =>
                                pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                            }
                          case _ =>
                            pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                        }
                        if(found){
                          clause = modifyCaseClause(elems, getCaseClause(elems), a, value.toString)
                          pattern += "case " + clause + " => " + s"$stat" + "\n"
                        }

                      case _ =>
                        pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                    }
                  case _ =>
                    pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
                }
              case _ =>
                pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
            }

          case _ =>
            pattern += "case " + clause + " if(" + pred + ") => " + s"$stat" + "\n"
        }

      }
      else {
        pattern += "case " + clause + " => " + s"$stat" + "\n"
      }
    }
    pattern += "}\n"
    var tree = s"$pattern".parse[Term].get
    tree
    //println(tree.toString())
  }

  /**
   * based on ifMap size, refactor code or call patternMatchExpression to do so.
   * @param ifMap
   * @return refactored program tree and note
   */
  def ifelseRefactor(ifMap: scala.collection.mutable.LinkedHashMap[String, String]): (scala.meta.Tree, String) = {
    var pattern = ""
    if (ifMap.size == 2) {
      for ((pred, stat) <- ifMap) {
        if (pred != "else") {
          pattern += s"$pred" + " match " + "{\n" + " case true => " + s"$stat" + "\n"
        }
        else {
          pattern += " case false => " + s"$stat" + "\n"
        }
      }
      pattern += "}"
      var tree = s"$pattern".parse[Term].get
      (tree, "pattern")
      //println(tree.toString())
    }
    else {
      var tupleElements = tupleSet(ifMap)
      if(tupleElements.nonEmpty && tupleElements.size<4)
        (patternMatchExpression(ifMap, tupleElements), "pattern")
      else
        (q"list.f","not")
        //println("Pattern Matching not worth it.")
    }
  }

  /**
   * Build the ifMap which is key to refactoring, call ifelseRefactor on the map and
   * return the refactored code and note to the print function of output object.
   * @param if_statement the tree structure of a nested if-else statement.
   */
  def classifyRefactoring(if_statement: (scala.meta.Tree, scala.meta.Tree, scala.meta.Tree)): Unit = {
    var ifMap = scala.collection.mutable.LinkedHashMap.empty[String, String]
    var if_predicate = if_statement._1.toString()
    var if_body = if_statement._2.toString()
    var code = ""
    var else_body = if_statement._3
    ifMap += (if_predicate -> if_body)
    code += "if(" + if_predicate + ")\n" + " " + if_body + "\n"
    var loop = true
    while (loop) {
      else_body match {
        case Term.If(node) =>
          var pred = node._1.toString()
          var stat = node._2.toString()
          ifMap += (pred -> stat)
          code += "else if(" + pred + ")\n" + " " + stat + "\n"
          else_body = node._3
        case _ =>
          loop = false
      }
    }
    ifMap += ("else" -> else_body.toString())
    code += "else" + "\n" + " " + else_body.toString() + "\n"

    var ref = ifelseRefactor(ifMap)._1
    var note = ifelseRefactor(ifMap)._2
    var codeT = code.parse[Term].get
    output.printRefactorings(codeT, ref, note)

  }

}
