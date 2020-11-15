package events

import model.{Pairing, Scenario, StraightSeason}

object MatchUpCeremony {
  def register(season: StraightSeason, guess: Scenario, numberCorrect: Int): StraightSeason = {
    val unknownPairings = guess
      .pairs
      .filter(p => !season.perfectMatches.contains(p) && !season.noMatches.contains(p))
    val newPerfectMatches = newMatches(season, guess, numberCorrect, unknownPairings)
    season
      .copy(scenarios = newScenarios(season, guess, numberCorrect))
      .updateWithInfo(newPerfectMatches, newNoMatches(season, guess, numberCorrect, unknownPairings, newPerfectMatches))
  }

  private def newScenarios(season: StraightSeason, guess: Scenario, numberCorrect: Int): Set[Scenario] =
    season.scenarios.filter(s => s.numberMatching(guess) == numberCorrect)

  private def newNoMatches(season: StraightSeason, guess: Scenario, numberCorrect: Int, unknownPairingsInGuess: Set[Pairing], newMatches: Set[Pairing]): Set[Pairing] = {
     val noMatches = if (numberCorrect - season.perfectMatches.count(guess.contains) == 0)
      unknownPairingsInGuess
    else
      Set[Pairing]()
    noMatches | newNoMatchesFromNewMatches(newMatches, season)
  }

  private def newMatches(season: StraightSeason, guess: Scenario, numberCorrect: Int, unknownPairingsInGuess: Set[Pairing]): Set[Pairing] = {
    if (numberCorrect - season.perfectMatches.size == unknownPairingsInGuess.size)
      unknownPairingsInGuess
    else
      Set()
  }

  private def newNoMatchesFromNewMatches(newMatches: Set[Pairing], season: StraightSeason): Set[Pairing] =
    newMatches.flatMap{case Pairing(woman, man) =>
      season.contestants.men.filter(_ != man).map(Pairing(woman, _)) |
        season.contestants.women.filter(_ != woman).map(Pairing(_, man))
    }
}
