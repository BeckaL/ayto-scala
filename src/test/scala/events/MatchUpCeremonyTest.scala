package events

import model.{AytoFixtures, Pairing, Scenario}
import org.scalatest.{FlatSpec, Matchers}

class MatchUpCeremonyTest extends FlatSpec with Matchers with AytoFixtures {
  "Match up ceremony" should "register a match up ceremony correctly when all are no matches" in {
    val pairs = pairsFrom(("a", "d"), ("b", "e"), ("c", "f"))
    val newSeason = MatchUpCeremony.register(threePairSeason, Scenario(pairs), 0)

    val expectedScenarios = Set(
      Scenario.from(List(("a", "e"), ("b", "f"), ("c", "d"))),
      Scenario.from(List(("a", "f"), ("b", "d"), ("c", "e"))),
    )
    val expectedNoMatches = pairs


    newSeason.perfectMatches shouldBe Set.empty
    newSeason.scenarios shouldBe expectedScenarios
    newSeason.noMatches shouldBe expectedNoMatches
  }

  "Match up ceremony" should "register a match up ceremony correctly when there is no information derived" in {
    val pairs = pairsFrom(("a", "d"), ("b", "e"), ("c", "f"))
    val newSeason = MatchUpCeremony.register(threePairSeason, Scenario(pairs), 1)

    val expectedScenarios = Set(
      Scenario.from(List(("a", "d"), ("b", "f"), ("c", "e"))),
      Scenario.from(List(("a", "e"), ("b", "d"), ("c", "f"))),
      Scenario.from(List(("a", "f"), ("b", "e"), ("c", "d")))
    )

    newSeason.scenarios shouldBe expectedScenarios
    newSeason.noMatches shouldBe Set.empty
  }

  "Match up ceremony" should "register a match up ceremony correctly when perfect matches can be derived" in {
    val pairs = pairsFrom(("a", "w"), ("b", "x"), ("c", "y"), ("d", "z"))
    val seasonBeforeMatchUp = fourPairSeason.copy(noMatches = pairsFrom(("a", "w"), ("d", "z")))
    val newSeason = MatchUpCeremony.register(seasonBeforeMatchUp, Scenario(pairs), 2)

    newSeason.perfectMatches shouldBe pairsFrom(("b", "x"), ("c", "y"))
    newSeason.noMatches shouldBe pairsFrom(("a", "x"), ("c", "x"), ("d", "x"), ("b", "y"), ("b", "w"), ("b", "z"), ("a", "y"), ("d", "y"), ("c", "w"), ("c", "x"), ("c", "z"), ("d", "z"), ("a", "w"))
  }
}
