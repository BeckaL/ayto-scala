package model

trait ProbabilityResult

case class ComleteProbabilityForWoman(woman: String, probabilitiesForMen: Map[String, Double]) extends ProbabilityResult

case class IncompleteProbabilityForWoman(woman: String, probabilitiesForMen: Map[String, Option[Double]]) extends ProbabilityResult
