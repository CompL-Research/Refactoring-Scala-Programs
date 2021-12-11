package refactoring_tool.loop_replacer

import scala.meta._
import scala.meta.internal.javacp.BaseType.S

/**
 *  The forLoop object contains functions which are
 *  used to refactor forLoops and return new
 *  metaprograms which are Scala inbuilt methods
 */
object forLoop {

  /**
   *
   * @param listName the original list over which loop was iterating
   * @param itr the iterator variable of the loop
   * @param loopBody the loop body in tree format
   * @return refactored tree which will be converted into scala program and presented to the user
   * @return note explains to the user the type of refactoring
   *  The function identifies based on repeated and intricate pattern-matching if
   *  the loop meets the criteria to be converted into a reduce, map, or foreach method of scala.
   */
  def loopBody(listName: String, itr: String, loopBody: scala.meta.Tree): (scala.meta.Tree, String) = {
    var ref = q"""list.f()"""
    var note = ""
    val itrName = Term.Name(s"$itr") //x
    val itrPat = Pat.Var(itrName)
    val list = Term.Name(s"$listName") //xs

    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {

        case q"$sum += $expr" =>
          if(expr.toString()==itr) {
            ref = q"""$list.reduce((x,y) => x+y)"""
            note = "reduce"
          }

          else{
            s"$expr".parse[Term].get  match {
              case q"$function($it)" =>
                ref = q"""$list.map(x => $function(x))"""
                note = "map"

              case _ =>
                note = "mapS"

            }
          }

        case q"$func($it)" =>
          ref = q"""$list.foreach($func)"""
          note = "foreach"

        case node =>
          super.apply(node)

      }
    }
    traverser(loopBody)
    (ref, note)

  }


  /**
   *
   * @param listName the original list over which loop was iterating
   * @param itr the iterator variable of the loop
   * @param loopBody the loop body in tree format
   * @return refactored tree which will be converted into scala program and presented to the user
   * @return note explains to the user the type of refactoring
   *  The function identifies based on repeated and intricate pattern-matching if
   *  the loop meets the criteria to be converted into a flatMap, or flatten method of scala.
   */
  def loopBodyLoop(listName: String, itr: String, loopBody: scala.meta.Tree): (scala.meta.Tree, String) = {
    var ref = q"""list.f"""
    var ref1 = q"list.f()"
    var note = ""
    val itrName = Term.Name(s"$itr") //x
    val list = Term.Name(s"$listName") //xs

    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {

        case Term.For(loop) =>
          var enumerator = loop._1.head.toString()

          var list2 = enumerator.substring(enumerator.indexOf("<-")+"<- ".length, enumerator.length)
          var itr2= enumerator.substring(0, enumerator.indexOf(" "))
          if (itr == list2) {
            loop._2 match {
              case q"$sum += $expr" =>
                if(expr.toString()==itr2) {
                  ref = q"""$list.flatten"""
                  note ="flatten"
                }

              case _ =>
                note = "flattenS"

            }
          }
          else {
            Term.Name(s"$list2") match {
              case q"""$func($itrx)""" =>
                if(itrx.toString()==itr)
                  loop._2 match {
                    case q"$sum += $expr" =>
                      if(expr.toString()==itr2) {
                        ref1 = q"$list.flatMap(x => $func(x))"
                        note ="flatMap"
                      }

                    case _ =>
                      note ="flatMapS"
                  }

              case _ =>
                note ="flatMapS"
            }
          }

        case node =>
          super.apply(node)

      }
    }
    traverser(loopBody)
    if(note=="flatMap" || note=="flatMapS")
      (ref1, note)
    else
      (ref, note)
  }

  /**
   *
   * @param listName the original list over which loop was iterating
   * @param itr the iterator variable of the loop
   * @param loopBody the loop body in tree format
   * @return refactored tree which will be converted into scala program and presented to the user
   * @return note explains to the user the type of refactoring
   *  The function identifies based on repeated and intricate pattern-matching if
   *  the loop meets the criteria to be converted into a distinct method of scala.
   */
  def loopBodyLoopConditional(listName: String, itr: String, loopBody: scala.meta.Tree): (scala.meta.Tree, String) = {
    var ref = q"""list.f"""
    var note = ""
    val itrName = Term.Name(s"$itr") //x
    val list = Term.Name(s"$listName") //xs

    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Term.For(loop) =>
          var enumerator = loop._1.head.toString()
          var list2 = enumerator.substring(enumerator.indexOf("<-")+"<- ".length, enumerator.length)
          var itr2= enumerator.substring(0, enumerator.indexOf(" "))
          loop._2 match {
            case Term.If(cond) =>
              cond._1 match {
                case q"$a==$b" =>
                  if((a.toString()== itr && b.toString()== itr2) || (a.toString()== itr2 && b.toString()== itr))
                    note = "distinctS"

                case _ =>
                  note = "flattenS"

              }

            case _ =>
              note = ""

          }

        case Term.If(cond) =>
          cond._2 match {
            case q"$sum += $expr" =>
              if(expr.toString()==itr && note=="distinctS"){
                note="distinct"
                ref = q"$list.distinct"
              }

            case _ =>
              note = "distinctS"
          }

        case node =>
          super.apply(node)

      }
    }
    traverser(loopBody)
    (ref, note)
  }

  /**
   *
   * @param listName the original list over which loop was iterating
   * @param itrName the iterator variable of the loop
   * @param loopBody the loop body in tree format
   * @return refactored tree which will be converted into scala program and presented to the user
   * @return note explains to the user the type of refactoring
   *  The function identifies based on repeated and intricate pattern-matching if
   *  the loop meets the criteria to be converted into a filter,filterNot,map+filter,filter+foreach,contains,count, or distinct method of scala.
   */
  def loopBodyConditional(listName: String, itrName: String, loopBody: scala.meta.Tree): (scala.meta.Tree, String) = {
    var ref = q"""list.f()"""
    var ref2 = q"list.f"
    var note = ""
    val itr = Term.Name(s"$itrName") //x
    val list = Term.Name(s"$listName") //xs

    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {

        case Term.If(cond) =>
          cond._1 match {

            case q"!$l.contains($it)" =>
              if(it.toString()==itrName){
                ref2 = q"$list.distinct"
                note = "distinct"

              }

            case q"$func($it)" =>
              if(it.toString()==itrName){

                cond._2 match {
                  case q"$sum += $expr" =>
                    if(expr.toString()==itrName) {
                      ref = q"""$list.filter($func)"""
                      note = "filter"
                    }
                    else{
                      q"$expr" match {
                        case q"$fun($it)" =>
                          if(it.toString()==itrName) {
                            ref = q"""$list.filter($func).map(x=>$fun(x))"""
                            note = "filter+map"
                          }

                        case _ =>
                          note = "filter+mapS"
                      }
                    }

                  case q"$fun($it)" =>
                    if(it.toString()==itrName) {
                      ref = q"""$list.filter($func).foreach($fun)"""
                      note = "filter+foreach"
                    }

                  case _ =>
                }
              }
              else
                note = "filterS"

            case q"!$func($it)" =>
              if(it.toString()==itrName){
                cond._2 match {
                  case q"$sum += $expr" =>
                    if(expr.toString()==itrName) {
                      ref = q"""$list.filterNot($func)"""
                      note = "filterNot"
                    }
                    else{
                      q"$expr" match {
                        case q"$fun($it)" =>
                          if(it.toString()==itrName) {
                            ref = q"""$list.filterNot($func).map(x=>$fun(x))"""
                            note = "filterNot"
                          }

                        case _ =>
                          note = "filter+mapS"
                      }
                    }

                  case q"$fun($it)" =>
                    if(it.toString()==itrName) {
                      ref = q"""$list.filterNot($func).foreach($fun)"""
                      note = "filterNot"
                    }

                  case _ =>
                }
              }
              else
                note = "filterS"

            case q"$it == $x" =>
              if(it.toString()==itrName){
                cond._2 match {
                  case q"$cn = $c +1" =>
                    ref = q"""$list.count(_==$x)"""
                    note = "count"

                  case q"$c += 1" =>
                    ref = q"""$list.count(_==$x)"""
                    note = "count"

                  case _ =>
                }
              }

            case _ =>

          }

        case node =>
          super.apply(node)

      }
    }
    traverser(loopBody)
    if(note=="distinct")
      (ref2, note)
    else
      (ref, note)
  }

  /**
   *
   * @param listName the original list over which loop was iterating
   * @param itrName the iterator variable of the loop
   * @param loopBody the loop body in tree format
   * @return refactored tree which will be converted into scala program and presented to the user
   * @return note explains to the user the type of refactoring
   *  The function identifies based on repeated and intricate pattern-matching if
   *  the loop meets the criteria to be converted into a lastIndexOf or lastIndexWhere method of scala.
   */
  def indexBased(listName: String, itrName: String, loopBody: scala.meta.Tree): (scala.meta.Tree, String) = {
    var ref = q"""list.f()"""
    var note = ""
    val itr = Term.Name(s"$itrName") //x
    val list = Term.Name(s"$listName") //xs

    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Term.If(cond) =>
          cond._2 match {
            case q"$index = $i" =>
              if(i.toString()==itrName){
                cond._1 match {
                  case q"""$lst($it)==$l""" =>
                    if(lst.toString()==listName && it.toString()==itrName){
                      note = "lastIndexOf"
                      ref = q"$list.lastIndexOf($l)"
                    }
                  case q"$func($lst($it))" =>
                    if(lst.toString()==listName && it.toString()==itrName){
                      note = "lastIndexWhere"
                      ref = q"$list.lastIndexWhere(x => $func(x))"
                    }

                  case _ =>
                    note = "lastIndexWhereS"
                }
              }

            case _ =>

          }

        case node =>
          super.apply(node)
      }
    }
    traverser(loopBody)
    (ref, note)
  }

  /**
   *
   * @param loop is the body component of a scalameta tree node for a for-loop.
   * @return typ is the number indicating the type of refactoring.
   * The function analyses the loop body based on presense of an internal loop
   * or conditional or both and classifies it.
   */
  def classifyLoopBody(loop: scala.meta.Tree): Int = {
    var typ=1
    val traverser = new Traverser {
      override def apply(tree: Tree): Unit = tree match {

        case Term.For(l) =>
          l._2 match {
            case Term.If(i) =>
              typ =3
            case _ =>
              typ=2
          }

        case Term.If(c) =>
          if(typ!=2 && typ!=3)
          typ=4

        case node =>
          super.apply(node)

      }
    }
    traverser(loop)
    typ
  }

  /**
   *
   * @param loop is the scalameta object which one gets after parsing a code
   *             into a scalameta tree. It is how a loop node is represented in scalameta.
   * The for-loop refactorings have been broadly classified in 5 types, depending
   * on the structuring of the loop enumerator  and body. Based on the type
   * a different function is called to refactor in each case.
   */
  def classifyRefactoring(loop: (List[Enumerator],scala.meta.Tree)): Unit = {
    var enumerator = loop._1.head.toString()
    val loop_body = (loop._2).toString()
    val iterator = enumerator.substring(0, enumerator.indexOf(" ")) //x or i
    var original_list = enumerator.substring(enumerator.indexOf("<-")+"<- ".length, enumerator.length) //xs or xs.indices

    var originalLoop = s"for($iterator <- $original_list) $loop_body".parse[Term].get

    if(original_list.contains(".indices")){
      val listName = original_list.substring(0,original_list.length-".indices".length)
      var note = indexBased(listName, iterator,loop._2)._2
      var ref = indexBased(listName, iterator,loop._2)._1
      output.printRefactorings(originalLoop,ref,note)
    }
    else{
      classifyLoopBody(loop._2) match {
        case 1 =>
          var ref = loopBody(original_list,iterator,loop._2)._1
          var note = loopBody(original_list,iterator,loop._2)._2
          output.printRefactorings(originalLoop,ref,note)
        case 2 =>
          var ref = loopBodyLoop(original_list,iterator,loop._2)._1
          var note = loopBodyLoop(original_list,iterator,loop._2)._2
          output.printRefactorings(originalLoop,ref,note)
        case 3 =>
          var ref =loopBodyLoopConditional(original_list,iterator,loop._2)._1
          var note = loopBodyLoopConditional(original_list,iterator,loop._2)._2
          output.printRefactorings(originalLoop,ref,note)
        case 4 =>
          var ref = loopBodyConditional(original_list,iterator,loop._2)._1
          var note = loopBodyConditional(original_list,iterator,loop._2)._2
          output.printRefactorings(originalLoop,ref,note)
      }
    }

  }


  /**
   *
   * @param loops is a list of for-loops in the format (loop-enumerator,loop-body)
   *  it calls classifyRefactoring function for each list element.
   */
  def refactor(loops: List[(List[Enumerator],scala.meta.Tree)]): Unit = {
    loops.foreach((f:(List[Enumerator],scala.meta.Tree))=>classifyRefactoring(f))
  }


}
