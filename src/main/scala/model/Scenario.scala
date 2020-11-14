package model

case class Scenario(pairs: Set[Pairing]) {
  def contains(pair: Pairing): Boolean = pairs.contains(pair)
}

object Scenario {
  def from(pairs: List[(String, String)]):Scenario =
    Scenario(pairs.map{case(woman, man) => Pairing(woman, man)}.toSet)
}
