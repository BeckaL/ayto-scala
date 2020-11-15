package events

import model.{ConfirmedInfo, Pairing, Scenario, StraightSeason}

object MatchUpCeremony {
  def register(season: StraightSeason, guess: Scenario, numberCorrect: Int): StraightSeason =
    season
      .copy(scenarios = season.scenarios.filter(s => s.numberMatching(guess) == numberCorrect))
      .updateWithInfo(newConfirmedInfo(season, guess, numberCorrect))


  private def newConfirmedInfo(season: StraightSeason, guess: Scenario, numberCorrect: Int): ConfirmedInfo = {
    val unknownPairings = unknownPairingsInGuess(guess, season)
    val allUnknownsAreNoMatches = numberCorrect - season.confirmedInfo.perfectMatches.count(guess.contains) == 0
    val allUnknownsAreMatches = numberCorrect - season.confirmedInfo.perfectMatches.size == unknownPairings.size

    if (allUnknownsAreNoMatches) { ConfirmedInfo(Set(), unknownPairings)
    }
    else if (allUnknownsAreMatches) {
      ConfirmedInfo(unknownPairings, newNoMatchesFromNewMatches(unknownPairings, season))
    } else
      ConfirmedInfo.empty
  }

  private def unknownPairingsInGuess(guess: Scenario, season: StraightSeason): Set[Pairing] = guess
    .pairs
    .filter(p => !season.confirmedInfo.perfectMatches.contains(p) &&
      !season.confirmedInfo.noMatches.contains(p)
    )

  private def newNoMatchesFromNewMatches(newMatches: Set[Pairing], season: StraightSeason): Set[Pairing] =
    newMatches.flatMap{case Pairing(woman, man) =>
      season.contestants.men.filter(_ != man).map(Pairing(woman, _)) |
        season.contestants.women.filter(_ != woman).map(Pairing(_, man))
    }
}
