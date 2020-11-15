package events

import model.{Pairing, StraightSeason}

object TruthBooth {
  def register(season: StraightSeason, pairing: Pairing, result: Boolean): StraightSeason = result match {
    case true => perfectMatchTruthBooth(pairing, season)
    case false => noMatchTruthBooth(pairing, season)
  }

  private def noMatchTruthBooth(noMatch: Pairing, season: StraightSeason): StraightSeason = {
    val newScenarios = season.scenarios.filter(s => !s.contains(noMatch))
    season.copy(scenarios = newScenarios).updateWithInfo(Set[Pairing](), Set(noMatch))
  }

  private def perfectMatchTruthBooth(perfectMatch: Pairing, season: StraightSeason): StraightSeason = {
    val newScenarios = season.scenarios.filter(s => s.contains(perfectMatch))
    val newNoMatches: Set[Pairing] = season.possiblePairings
      .filter(p => p != perfectMatch && (p.woman == perfectMatch.woman || p.man == perfectMatch.man))
    val newPerfectMatches = Set(perfectMatch)
    season.copy(scenarios = newScenarios).updateWithInfo(newPerfectMatches, newNoMatches)
  }
}