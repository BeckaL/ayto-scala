package userInput

trait InputOutput {
  def getInput(msg: String): String
  def print(msg: String): Unit
}
