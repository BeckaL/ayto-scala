package model

trait StraightSeason[A] {
  val name: String
  val contestants: Contestants
  val weekNumber: Int
  val scenarios: A
  val possiblePairings: Set[Pairing]
  val confirmedInfo: ConfirmedInfo
}

final case class ConfirmedInfo(perfectMatches: Set[Pairing], noMatches: Set[Pairing])
object ConfirmedInfo {
  val empty = ConfirmedInfo(Set(), Set())
}