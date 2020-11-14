package events

import model.{Pairing, Scenario, StraightSeason}

object MatchUpCeremony {
  def register(season: StraightSeason, guess: Scenario, numberCorrect: Int): StraightSeason =
    season.copy(
      scenarios = newScenarios(season, guess, numberCorrect),
      noMatches = newNoMatches(season, guess, numberCorrect))

  private def unknownPairingsInGuess(guess: Scenario, season: StraightSeason) =
    guess.pairs.filter(p => !season.perfectMatches.contains(p) && !season.noMatches.contains(p))


  private def newScenarios(season: StraightSeason, guess: Scenario, numberCorrect: Int): Set[Scenario] =
    season.scenarios.filter(s => s.numberMatching(guess) == numberCorrect)

  private def newNoMatches(season: StraightSeason, guess: Scenario, numberCorrect: Int): Set[Pairing] =
    if (numberCorrect - season.perfectMatches.count(guess.contains) == 0)
      season.noMatches | unknownPairingsInGuess(guess, season)
    else
      season.noMatches
}
