package events

import model.{Pairing, StraightSeason}

object TruthBooth {
  def register(season: StraightSeason, pairing: Pairing, result: Boolean): StraightSeason = result match {
    case true => perfectMatchTruthBooth(pairing, season)
    case false => noMatchTruthBooth(pairing, season)
  }

  private def noMatchTruthBooth(noMatch: Pairing, season: StraightSeason): StraightSeason = {
    val newScenarios = season.scenarios.filter(s => !s.contains(noMatch))
    season.copy(scenarios = newScenarios, noMatches = season.noMatches + noMatch)
  }

  private def perfectMatchTruthBooth(perfectMatch: Pairing, season: StraightSeason): StraightSeason = {
    val newScenarios = season.scenarios.filter(s => s.contains(perfectMatch))
    val newNoMatches: Set[Pairing] =  season.noMatches | season.possiblePairings
      .filter(p => p != perfectMatch && (p.woman == perfectMatch.woman || p.man == perfectMatch.man))
    val newPerfectMatches = season.perfectMatches + perfectMatch
    season.copy(scenarios = newScenarios, perfectMatches = newPerfectMatches, noMatches = newNoMatches)
  }
}