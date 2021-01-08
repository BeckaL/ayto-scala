package userInput

import scala.util.{Success, Try}

class ActionSelector(inputOutput: InputOutput) extends Action with InputHelpers {
  override val inOut: InputOutput = inputOutput

  def getAction: Action = {
    val prompt = "Choose your action: 1 for inputting a truth booth, 2 for inputting a match up ceremony or 3 to quit"
    Try(inputOutput.getInput(prompt).toInt) match {
      case Success(1) => new TruthBoothInputter(inputOutput)
      case Success(2) => new MatchUpCeremonyInputter(inputOutput)
      case Success(3) => Quit
      case _ => inputOutput.print("I didn't understand that. Try again"); this
    }
  }
}