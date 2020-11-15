package events

import model._

object ProbabilitiesReconciler {
  def reconcile(table: ProbabilityTable, season: StraightSeason): StraightSeason = table match {
    case IncompleteProbabilityTable(rows) =>
      reconcile[Option[Double]](rows.map(_.asInstanceOf[ProbabilityResult[Option[Double]]]), season, incompleteConditions)
    case CompleteProbabilityTable(rows) =>
      reconcile[Double](rows.map(_.asInstanceOf[ProbabilityResult[Double]]), season, completeConditions)
  }

  private def reconcile[A](probabilities: Set[ProbabilityResult[A]], season: StraightSeason, conditions: (A => Boolean, A => Boolean)): StraightSeason = {
    val (newMatches, newNoMatches) =  (
      pairsWhereProbabilityIs(probabilities, conditions._1),
      pairsWhereProbabilityIs(probabilities, conditions._2)
    )
    season.updateWithInfo(newMatches, newNoMatches)
  }

  private def pairsWhereProbabilityIs[A](probabilities: Set[ProbabilityResult[A]], condition: A => Boolean) =
    probabilities.flatMap(p => p.probabilitiesForMen.collect{case (man, prob) if condition(prob) => man}.map(m => Pairing(p.woman, m)))

  private lazy val completeConditions = ((x: Double) => x == 1.0, (x: Double) => x == 0.0)
  private lazy val incompleteConditions = ((x: Option[Double]) => x.contains(1.0), (x: Option[Double]) => x.contains(0.0))
}