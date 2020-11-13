package model

case class Scenario(pairs: Set[Pairing])

object Scenario {
  def from(pairs: List[(String, String)]):Scenario =
    Scenario(pairs.map{case(woman, man) => Pairing(woman, man)}.toSet)
}
