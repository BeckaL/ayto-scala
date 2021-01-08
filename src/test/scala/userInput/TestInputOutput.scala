package userInput

class TestInputOutput(var instructions: List[String]) extends InputOutput {
  var output = List[String]()
  override def getInput(msg: String): String = {
    val instruction = instructions.head
    instructions = instructions.tail
    instruction
  }

  override def print(msg: String): Unit = output = output :+ msg
}

