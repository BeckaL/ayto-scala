package model

case class ProbabilityForWoman(woman: String, probabilitiesForMen: Map[String, Double])

case class UncertainProbabilityForWoman(woman: String, probabilitiesForMen: Map[String, Option[Double]])
