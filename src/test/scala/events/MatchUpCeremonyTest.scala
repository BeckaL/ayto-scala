package events

import model.{AytoFixtures, Scenario}
import org.scalatest.{FlatSpec, Matchers}

class MatchUpCeremonyTest extends FlatSpec with Matchers with AytoFixtures {
  "Match up ceremony" should "register a match up ceremony correctly" in {
    val pairs = pairsFrom(("a", "d"), ("b", "e"), ("c", "f"))
    val newSeason = MatchUpCeremony.register(basicSeason, Scenario(pairs), 0)

    val expectedScenarios = Set(
      Scenario.from(List(("a", "e"), ("b", "f"), ("c", "d"))),
      Scenario.from(List(("a", "f"), ("b", "d"), ("c", "e"))),
    )
    val expectedNoMatches = pairs

    newSeason.scenarios shouldBe expectedScenarios
    newSeason.noMatches shouldBe expectedNoMatches
  }
}
