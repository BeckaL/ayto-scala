package userInput

import model.InMemoryStraightSeason

class SeasonInputter(inputOutput: InputOutput) {
  def getStraightSeason: InMemoryStraightSeason =
    InMemoryStraightSeason.from(getSeasonName, getNames("women"), getNames("men"))

  private def getNames(gender: String): Set[String] = {
    inputOutput.getInput(s"Enter the names for the $gender, separated by commas").split(",").map(_.trim).toList match {
      case _ :: Nil => inputOutput.print("That doesn't look right to me, did you comma separate?")
        getNames(gender)
      case _ @ names => names.toSet
    }
  }

  private def getSeasonName: String = inputOutput.getInput("Enter season name:") match {
    case "" => inputOutput.print("Can't have an empty name. Try again"); getSeasonName
    case _ @ name => name
  }

}
