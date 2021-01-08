package userInput

import model.{Pairing, Scenario, InMemoryStraightSeason}

import scala.util.{Success, Try}

class MatchUpCeremonyInputter(inputOutput: InputOutput) extends Action with InputHelpers {
  override val inOut: InputOutput = inputOutput
  def run(season: InMemoryStraightSeason): (Scenario, Int) = {
    val (womenWithIndex, menWithIndex) = namesWithIndexes(season)
    val scenario = getAllCouples(womenWithIndex, menWithIndex, Set())
    val numberCorrect = getNumberCorrect(scenario.pairs.size)
    confirm(scenario, numberCorrect, season)
  }

  def confirm(scenario: Scenario, numberCorrect: Int, season: InMemoryStraightSeason): (Scenario, Int) = {
    val prompt = "Does this look right? Answer 'y' or 'n'" + scenario.pairs.map(pairing => s"${pairing.woman} + ${pairing.man}").mkString("\n") + s"\nnumber correct: $numberCorrect}"
    val yOrNinOut = inOut.getInput(prompt)
    yOrNinOut.toLowerCase match {
      case "y" => (scenario, numberCorrect)
      case "n" => run(season)
      case _ =>  inputOutput.print("I didn't understand that."); confirm(scenario, numberCorrect, season)
    }
  }


  private def getNumberCorrect(numberOfCouples: Int): Int = {
    Try(inOut.getInput("How many couples were correct?").toInt) match {
      case Success(i) if i <= numberOfCouples & i >= 0 => i
      case Success(_) =>
        inOut.print("That looks like too many couples to me, try again")
        getNumberCorrect(numberOfCouples)
      case _ =>
        inOut.print("That doesn't look like a valid number to me, try again")
        getNumberCorrect(numberOfCouples)
    }
  }

  private def getAllCouples(womenWithIndex: NamesToIndex, menWithIndex: NamesToIndex, currentCouples: Set[Pairing]): Scenario = {
    val c = getCouple(womenWithIndex, menWithIndex)
    val newCouples = currentCouples + c
    if (newCouples.size == womenWithIndex.size) {
      Scenario(newCouples)
    } else {
      getAllCouples(wipeNameIfChosen(womenWithIndex, c.woman), wipeNameIfChosen(menWithIndex, c.man), newCouples)
    }
  }

  private def wipeNameIfChosen(namesWithIndex: NamesToIndex, chosenPerson: String): NamesToIndex =
    namesWithIndex.map{case (name, i) => if(name == chosenPerson) ("", i) else (name, i)}

}
