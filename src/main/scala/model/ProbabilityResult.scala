package model

trait ProbabilityTable

case class CompleteProbabilityTable(rows: Set[CompleteProbabilityRow]) extends ProbabilityTable

case class IncompleteProbabilityTable(rows: Set[IncompleteProbabilityRow]) extends ProbabilityTable

trait ProbabilityResult[A] {
  val woman: String
  val probabilitiesForMen: Map[String, A]
}

case class CompleteProbabilityRow(woman: String, probabilitiesForMen: Map[String, Double]) extends ProbabilityResult[Double]

case class IncompleteProbabilityRow(woman: String, probabilitiesForMen: Map[String, Option[Double]]) extends ProbabilityResult[Option[Double]]
