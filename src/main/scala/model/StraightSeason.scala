package model

import Math.Factorial

final case class StraightSeason(name: String,
                                contestants: Contestants,
                                weekNumber: Int,
                                scenarios: Set[Scenario],
                                possiblePairings: Set[Pairing],
                                confirmedMatches: Set[Pairing],
                                confirmedNoMatches: Set[Pairing]) {

  val initialNumberOfProbabilities = Factorial.factorial(contestants.men.size)
}

object StraightSeason {
  def from(seasonName: String, men: Set[String], women: Set[String]): StraightSeason = {
    val possible_pairings = create_possible_pairings(women, men)
    val initial_scenarios = create_initial_scenarios(women, men)
    val contestants = Contestants(women, men)
    StraightSeason(seasonName, contestants, 0, initial_scenarios, possible_pairings, Set.empty, Set.empty)
  }

  private def create_possible_pairings(women: Set[String], men: Set[String]): Set[Pairing] =
    for {
      woman <- women
      man <- men
    } yield Pairing(woman, man)

  private def create_initial_scenarios(women: Set[String], men: Set[String]): Set[Scenario] =
   men.toList.permutations.toList.map(women.toList.zip(_)).toSet.map(Scenario.from)
}


