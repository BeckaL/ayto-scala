package events

import model.{Pairing, CompleteProbabilityRow, ProbabilityResult, StraightSeason, IncompleteProbabilityRow}

object ProbabilitiesReconciler {
  def reconcileComplete(probabilities: Set[CompleteProbabilityRow], season: StraightSeason) = {
    val (newMatches, newNoMatches) =  (
      pairsWhereCompleteProbabilityIs(probabilities, (x: Double) => x == 1.0),
      pairsWhereCompleteProbabilityIs(probabilities, (x: Double) => x == 0.0)
    )
    season.copy(perfectMatches = season.perfectMatches | newMatches, noMatches = season.noMatches | newNoMatches)
  }

  def reconcileIncomplete(probabilities: Set[IncompleteProbabilityRow], season: StraightSeason) = {
    val (newMatches, newNoMatches) = (
      pairsWhereIncompleteProbabilityIs(probabilities, (x: Option[Double]) => x.contains(1.0)),
      pairsWhereIncompleteProbabilityIs(probabilities, (x: Option[Double]) => x.contains(0.0))
    )
    season.copy(perfectMatches = season.perfectMatches | newMatches, noMatches = season.noMatches | newNoMatches)
  }

  private def pairsWhereCompleteProbabilityIs(probabilities: Set[CompleteProbabilityRow], condition: Double => Boolean) =
    probabilities.flatMap(p => collectProbabilitiesWhere(condition, p.probabilitiesForMen).map(m => Pairing(p.woman, m)))

  private def pairsWhereIncompleteProbabilityIs(probabilities: Set[IncompleteProbabilityRow], condition: Option[Double] => Boolean) =
    probabilities.flatMap(p => collectProbabilitiesWhere(condition, p.probabilitiesForMen).map(m => Pairing(p.woman, m)))

  private def collectProbabilitiesWhere[A](f: A => Boolean, probabilitiesForMen: Map[String, A]) =
    probabilitiesForMen.collect{case (man, prob) if f(prob) => man}
}