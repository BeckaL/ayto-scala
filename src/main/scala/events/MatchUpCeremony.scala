package events

import model.{Pairing, Scenario, StraightSeason}

object MatchUpCeremony {
  def register(season: StraightSeason, guess: Scenario, numberCorrect: Int): StraightSeason = {
    val unknownPairings = guess
      .pairs
      .filter(p => !season.perfectMatches.contains(p) && !season.noMatches.contains(p))
    val newPerfectMatches = newMatches(season, guess, numberCorrect, unknownPairings)
    season.copy(
      scenarios = newScenarios(season, guess, numberCorrect),
      noMatches = newNoMatches(season, guess, numberCorrect, unknownPairings) | newNoMatchesFromNewMatches(newPerfectMatches, season),
      perfectMatches = newPerfectMatches)
  }

  private def newScenarios(season: StraightSeason, guess: Scenario, numberCorrect: Int): Set[Scenario] =
    season.scenarios.filter(s => s.numberMatching(guess) == numberCorrect)

  private def newNoMatches(season: StraightSeason, guess: Scenario, numberCorrect: Int, unknownPairingsInGuess: Set[Pairing]): Set[Pairing] =
    if (numberCorrect - season.perfectMatches.count(guess.contains) == 0)
      season.noMatches | unknownPairingsInGuess
    else
      season.noMatches

  private def newMatches(season: StraightSeason, guess: Scenario, numberCorrect: Int, unknownPairingsInGuess: Set[Pairing]): Set[Pairing] = {
    if (numberCorrect - season.perfectMatches.size == unknownPairingsInGuess.size)
      season.perfectMatches | unknownPairingsInGuess
    else
      season.perfectMatches
  }

  private def newNoMatchesFromNewMatches(newMatches: Set[Pairing], season: StraightSeason) =
    newMatches.flatMap{case Pairing(woman, man) =>
      season.contestants.men.filter(_ != man).map(Pairing(woman, _)) |
        season.contestants.women.filter(_ != woman).map(Pairing(_, man))
    }
}
