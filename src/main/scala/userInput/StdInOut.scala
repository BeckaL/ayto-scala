package userInput

import scala.io.StdIn.readLine

object StdInOut extends InputOutput {
  override def getInput(msg: String): String = readLine("\n" + msg + "\n")

  override def print(msg: String): Unit = println(msg)
}