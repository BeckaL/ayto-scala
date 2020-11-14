package events

import model.{AytoFixtures, Pairing, Scenario}
import org.scalatest.{FlatSpec, Matchers}

class TruthBoothTest extends FlatSpec with Matchers with AytoFixtures {
  "Truth booth" should "register a no match truth booth correctly" in {
    val newSeason = TruthBooth.register(basicSeason, Pairing("a", "d"), false)

    val expectedScenarios =
      Set(Scenario.from(List(("a", "e"),("b", "d"),("c", "f"))),
        Scenario.from(List(("a", "e"),("b", "f"),("c", "d"))),
        Scenario.from(List(("a", "f"),("b", "d"),("c", "e"))),
        Scenario.from(List(("a", "f"),("b", "e"),("c", "d"))))

    val expectedPerfectMatches = Set.empty
    val expectedNoMatches = pairsFrom(("a", "d"))

    newSeason.scenarios shouldBe expectedScenarios
    newSeason.perfectMatches shouldBe expectedPerfectMatches
    newSeason.noMatches shouldBe expectedNoMatches
    newSeason.contestants shouldBe basicSeason.contestants
    newSeason.name shouldBe basicSeason.name
    newSeason.weekNumber shouldBe basicSeason.weekNumber
  }


  it should "register a perfect match truth booth correctly" in {
    val newSeason = TruthBooth.register(basicSeason, Pairing("a", "d"), true)

    val expectedScenarios =
      Set(
        Scenario.from(List(("a", "d"),("b", "e"),("c", "f"))),
        Scenario.from(List(("a", "d"),("b", "f"),("c", "e"))),
      )

    val expectedPerfectMatches = Set(Pairing("a", "d"))
    val expectedNoMatches = pairsFrom(("a", "e"), ("a", "f"), ("b", "d"), ("c", "d"))

    newSeason.scenarios shouldBe expectedScenarios
    newSeason.perfectMatches shouldBe expectedPerfectMatches
    newSeason.noMatches shouldBe expectedNoMatches
    newSeason.contestants shouldBe basicSeason.contestants
    newSeason.name shouldBe basicSeason.name
    newSeason.weekNumber shouldBe basicSeason.weekNumber
  }
}
