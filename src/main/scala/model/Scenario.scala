package model

case class Scenario(pairs: Set[Pairing]) {
  def contains(pair: Pairing): Boolean = pairs.contains(pair)

  def numberMatching(scenario2: Scenario): Int = scenario2.pairs.count(this.contains)
}

object Scenario {
  def from(pairs: List[(String, String)]):Scenario =
    Scenario(pairs.map{case(woman, man) => Pairing(woman, man)}.toSet)
}
