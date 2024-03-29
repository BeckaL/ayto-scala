package events

import model.{ConfirmedInfo, Pairing, InMemoryStraightSeason}

object TruthBooth {
  def register(season: InMemoryStraightSeason, pairing: Pairing, result: Boolean): InMemoryStraightSeason = result match {
    case true => perfectMatchTruthBooth(pairing, season)
    case false => noMatchTruthBooth(pairing, season)
  }

  private def noMatchTruthBooth(noMatch: Pairing, season: InMemoryStraightSeason): InMemoryStraightSeason =
    season
      .copy(scenarios = season.scenarios.filter(s => !s.contains(noMatch)))
      .updateWithInfo(ConfirmedInfo(Set(), Set(noMatch)))

  private def perfectMatchTruthBooth(perfectMatch: Pairing, season: InMemoryStraightSeason): InMemoryStraightSeason = {
    val newScenarios = season.scenarios.filter(s => s.contains(perfectMatch))
    val newNoMatches: Set[Pairing] = season.possiblePairings
      .filter(p => p != perfectMatch && (p.woman == perfectMatch.woman || p.man == perfectMatch.man))
    val newPerfectMatches = Set(perfectMatch)
    season.copy(scenarios = newScenarios).updateWithInfo(ConfirmedInfo(newPerfectMatches, newNoMatches))
  }
}