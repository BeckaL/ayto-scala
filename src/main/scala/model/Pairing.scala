package model

case class Pairing(woman: String, man: String)

case class ProbabilityForWoman(woman: String, probabilitiesForMen: Map[String, Double])