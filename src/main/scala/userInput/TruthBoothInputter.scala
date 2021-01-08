package userInput

import model.{Pairing, InMemoryStraightSeason}

class TruthBoothInputter(inputOutput: InputOutput) extends Action with InputHelpers {
  override val inOut: InputOutput = inputOutput
  def getTruthBooth(season: InMemoryStraightSeason): (Pairing, Boolean) = {
    val (women, men) = namesWithIndexes(season)
    (getCouple(women, men), getResult)
  }

  private def getResult: Boolean =
    inputOutput.getInput("Type 't' for perfect match or 'f' for no match").toLowerCase match {
      case "t" => true
      case "f" => false
      case _ =>
        inputOutput.print("I didn't understand that. Try again")
        getResult
    }
}
