import events.{MatchUpCeremony, TruthBooth}
import model.{ProbabilityTable, StraightSeason}
import userInput.{InputOutput, MatchUpCeremonyInputter, Quit, SeasonInputter, StdInOut, TruthBoothInputter}

import scala.util.{Success, Try}

object Main {
    def main(args: Array[String]): Unit = {
        val season = new SeasonInputter(StdInOut).getStraightSeason

    }

  def selectAction(inputOutput: InputOutput, season: StraightSeason): Option[StraightSeason] = {
    val prompt = "Choose your action: 1 for inputting a truth booth, 2 for inputting a match"
    Try(inputOutput.getInput(prompt).toInt) match {
      case Success(1) =>
        val guess = new TruthBoothInputter(inputOutput).getTruthBooth(season)
        val updatedSeason = TruthBooth.register(season, guess._1, guess._2)
        Some(updatedSeason)
      case Success(2) =>
        val ceremony = new MatchUpCeremonyInputter(inputOutput).run(season)
        val updatedSeason = MatchUpCeremony.register(season, ceremony._1, ceremony._2)
        Some(updatedSeason)
      case Success(3) => None
      case _ => inputOutput.print("I didn't understand that. Try again"); selectAction(inputOutput, season)
    }
  }


  def calculateProbabilities(inputOutput: InputOutput, season: StraightSeason) : (StraightSeason, ProbabilityTable) = ???

}
