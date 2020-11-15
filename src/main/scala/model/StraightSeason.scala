package model

import factorialMaths.Factorial

final case class ConfirmedInfo(perfectMatches: Set[Pairing], noMatches: Set[Pairing])
object ConfirmedInfo {
  val empty = ConfirmedInfo(Set(), Set())
}

final case class StraightSeason(name: String,
                                contestants: Contestants,
                                weekNumber: Int,
                                scenarios: Set[Scenario],
                                possiblePairings: Set[Pairing],
                                confirmedInfo: ConfirmedInfo) {

  val initialNumberOfProbabilities: Int = Factorial.factorial(contestants.men.size)

  val hasNoConfirmedInformation = confirmedInfo.perfectMatches.isEmpty && confirmedInfo.noMatches.isEmpty
  val isSolved = confirmedInfo.perfectMatches.size == contestants.women.size

  def updateWithInfo(newConfirmedInfo: ConfirmedInfo): StraightSeason =
    this.copy(
      confirmedInfo = ConfirmedInfo(
        perfectMatches = this.confirmedInfo.perfectMatches | newConfirmedInfo.perfectMatches,
        noMatches = this.confirmedInfo.noMatches | newConfirmedInfo.noMatches
      )
    )
}

object StraightSeason {
  def from(seasonName: String, women: Set[String], men: Set[String]): StraightSeason = {
    val possible_pairings = create_possible_pairings(women, men)
    val initial_scenarios = create_initial_scenarios(women, men)
    val contestants = Contestants(women, men)
    StraightSeason(seasonName, contestants, 0, initial_scenarios, possible_pairings, ConfirmedInfo(Set.empty, Set.empty))
  }

  private def create_possible_pairings(women: Set[String], men: Set[String]): Set[Pairing] =
    for {
      woman <- women
      man <- men
    } yield Pairing(woman, man)

  private def create_initial_scenarios(women: Set[String], men: Set[String]): Set[Scenario] =
   men.toList.permutations.toList.map(women.toList.zip(_)).toSet.map(Scenario.from)
}


