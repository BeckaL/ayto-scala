import calculator.{StraightSeasonEstimatedProbabilityCalculator, StraightSeasonProbabilityCalculator}
import display.ProbabilityFormatter
import events.{MatchUpCeremony, ProbabilitiesReconciler, TruthBooth}
import model.InMemoryStraightSeason
import userInput.{MatchUpCeremonyInputter, SeasonInputter, StdInOut, TruthBoothInputter}

import scala.util.{Success, Try}

object Main {
  val inputOutput = StdInOut
    def main(args: Array[String]): Unit =
       run(new SeasonInputter(inputOutput).getStraightSeason)


  def run(season: InMemoryStraightSeason): Unit = {
    val updatedSeason = selectAction(season)
    updatedSeason match {
      case Some(s) => run(s)
      case None => println("goodbye")
    }
  }

  def selectAction(season: InMemoryStraightSeason): Option[InMemoryStraightSeason] = {
    val prompt = "Choose your action: 1 for inputting a truth booth, 2 for inputting a match up ceremony, or 3 to quit"
    Try(inputOutput.getInput(prompt).toInt) match {
      case Success(1) =>
        val guess = new TruthBoothInputter(inputOutput).getTruthBooth(season)
        val seasonAfterTruthBoothBasic = TruthBooth.register(season, guess._1, guess._2)
        val seasonAfterProbabilitiesReconciliation = displayProbabilitiesAndUpdateSeason(seasonAfterTruthBoothBasic)
        Some(seasonAfterProbabilitiesReconciliation)
      case Success(2) =>
        val ceremony = new MatchUpCeremonyInputter(inputOutput).run(season)
        val updatedSeason = MatchUpCeremony.register(season, ceremony._1, ceremony._2)
        val seasonAfterProbabilitiesReconciliation = displayProbabilitiesAndUpdateSeason(updatedSeason)
        Some(seasonAfterProbabilitiesReconciliation)
      case Success(3) => None
      case _ => inputOutput.print("I didn't understand that. Try again"); selectAction(season)
    }
  }

  def displayProbabilitiesAndUpdateSeason(season: InMemoryStraightSeason) : InMemoryStraightSeason = {
    val probabilityTable = if (season.scenarios.size > 350000) {
      inputOutput.print(s"number of scenarios is too big at ${season.scenarios.size} - only checking definite probabilities")
      StraightSeasonEstimatedProbabilityCalculator.calculate(season)
    } else {
      StraightSeasonProbabilityCalculator.calculate(season)
    }
    inputOutput.print(ProbabilityFormatter.format(probabilityTable))
    ProbabilitiesReconciler.reconcile(probabilityTable, season)
  }

}

// Adam,Chris S.,Chris T.,Dillan,Dre,Ethan,John,Joey,Ryan,Wes

//  Brittany,Ashleigh,Jessica,Coleysia,Jacy,Shanley,Simone,Paige,Amber,Kayla