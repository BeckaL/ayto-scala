package calculator

import model.{Pairing, ProbabilityForWoman, Scenario, StraightSeason}

object StraightSeasonProbabilityCalculator {
  def calculate(season: StraightSeason): Set[ProbabilityForWoman] =
    season.possiblePairings.groupBy(_.woman)
      .map{ case woman -> pairings =>
        ProbabilityForWoman(woman,
          pairings.map(p =>
            p.man -> calculateProbabilityForPair(p, season)
          ).toMap
        )
      }.toSet

  def calculateProbabilityForPair(pair: Pairing, season: StraightSeason): Double = pair match {
    case _ if season.perfectMatches.contains(pair) => 1.00
    case _ if season.noMatches.contains(pair) => 0.00
    case _ if season.weekNumber == 0 => to2decimalPlaces(1.0 / season.contestants.women.size)
    case _ => countScenariosForPair(pair, season.scenarios) / season.scenarios.size
  }

  private def countScenariosForPair(pair: Pairing, scenarios: Set[Scenario]): Double =
    scenarios.foldLeft(0.0)((runningTotal, scenario) => if(scenario.contains(pair)) runningTotal + 1 else runningTotal)

  private def to2decimalPlaces(n: Double) = Math.round(100.0 * n) / 100.0
}
