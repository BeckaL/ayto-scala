package calculator

import model.{Pairing, StraightSeason, IncompleteProbabilityRow}

object StraightSeasonEstimatedProbabilityCalculator {
  def calculate(season: StraightSeason): Set[IncompleteProbabilityRow] = {
    season.possiblePairings.groupBy(_.woman).map{ case woman -> pairings =>
      IncompleteProbabilityRow(woman, pairings.map(p => p.man -> estimateProbabilityForPair(p, season)).toMap)
    }.toSet

  }

  private def estimateProbabilityForPair(pair: Pairing, season: StraightSeason): Option[Double] = pair match {
    case _ if season.hasNoConfirmedInformation => Some(to2decimalPlaces(1.0 / season.contestants.women.size))
    case _ if season.confirmedInfo.perfectMatches.contains(pair) => Some(1.0)
    case _ if season.confirmedInfo.noMatches.contains(pair) => Some(0.0)
    case _ if season.confirmedInfo.perfectMatches.map(_.woman).contains(pair.woman) => Some(0.0)
    case _ if season.confirmedInfo.perfectMatches.map(_.man).contains(pair.man) => Some(0.0)
    case _ => None
  }

  private def to2decimalPlaces(n: Double): Double = Math.round(100.0 * n) / 100.0
}
