/**
 * the entire project package. Holds the main object for start up and
 * other packaged like if_else_replacer and loop_replacer.
 */
package refactoring_tool
import scala.meta._

/**
 * entry point for the project.
 */
object main extends App{

  /**
   * gets the code that needs to be refactored
   * from the file 'source_code.scala'. The code is parsed
   * into scalameta tree and passed to loop_replacer and
   * if_else_replacer for refactorings.
   */
  def getFromFile(): Unit = {
    val filepath = "src/main/scala/source_code.scala"
    val fileSource = scala.io.Source.fromFile(filepath)
    var code =""
    fileSource.getLines.foreach((f:String)=> code += f +"\n")
    val tree:scala.meta.Tree = code.parse[Source].get
    loop_replacer.checkLoop.isLoop(tree)
    if_else_replacer.checkConditonal.isConditional(tree)

  }

  /**
   * gets code that needs to refactored from terminal
   * and parses it into scalameta tree and passes it
   * to loop_replacer and if_else_replacer for refacotirng
   */
  def getFromTerminal(): Unit = {
    println("Enter code snippet (conditional expression/ loop/ definition):")
    var input =  Iterator.continually(scala.io.StdIn.readLine)
      .takeWhile(Option(_).fold(false)(_.nonEmpty)).toList
    var code =""
    input.foreach((f:String)=> code += f +"\n")
    val tree:scala.meta.Tree = code.parse[Stat].get
    loop_replacer.checkLoop.isLoop(tree)
    if_else_replacer.checkConditonal.isConditional(tree)
  }

  /**
   * The function sets up a chain for calling every function
   * in the project. When it is called it sets up the project,
   * asks user whether they want to read code from
   * source_code.scala or write in terminal. Based on the response,
   * getFromTerminal() or getFromFile() is called or the program
   * is terminated.
   */
  def getCode(): Unit = {
    println("Read from source_code.scala? y/n/t(terminate)")
    val input = scala.io.StdIn.readChar()
    input match {
      case 'y' => getFromFile()
      case 'n' => getFromTerminal()
      case 't' => System.exit(0)
      case _ => getCode()

    }
  }


  getCode()






}
