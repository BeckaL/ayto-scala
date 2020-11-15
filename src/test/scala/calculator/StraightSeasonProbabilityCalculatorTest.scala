package calculator

import model.{AytoFixtures, Pairing, CompleteProbabilityForWoman, Scenario, StraightSeason}
import org.scalatest.{FlatSpec, Matchers}

class StraightSeasonProbabilityCalculatorTest extends FlatSpec with Matchers with AytoFixtures {
  "StraightSeasonProbabilityCalculator" should "calculate probabilities from existing confirmed matches and no matches" in {
    val confirmedMatches = pairsFrom(("a", "d"), ("b", "e"), ("c", "f"))
    val confirmedNoMatches = pairsFrom(("a", "e"), ("a", "f"), ("b", "d"), ("b", "f"), ("c", "d"), ("c", "e"))
    val season = threePairSeason.copy(perfectMatches = confirmedMatches, noMatches = confirmedNoMatches)

    val expectedProbabilities = Set(
      CompleteProbabilityForWoman("a", Map("d" -> 1.00, "e" -> 0.00, "f" -> 0.00)),
      CompleteProbabilityForWoman("b", Map("d" -> 0.00, "e" -> 1.00, "f" -> 0.00)),
      CompleteProbabilityForWoman("c", Map("d" -> 0.00, "e" -> 0.00, "f" -> 1.00)))

    StraightSeasonProbabilityCalculator.calculate(season) shouldBe expectedProbabilities
  }

  it should "calculate probabilities when the week number is 0 and there is no confirmed information" in {
    val expectedProbabilities = Set(
      CompleteProbabilityForWoman("a", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)),
      CompleteProbabilityForWoman("b", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)),
      CompleteProbabilityForWoman("c", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)))

    StraightSeasonProbabilityCalculator.calculate(threePairSeason) shouldBe expectedProbabilities
  }


  it should "calculate probabilities when there is a mix of confirmed information and non confirmed information" in {
    val confirmedNoMatches = pairsFrom(("a", "e"))
    val scenarios = Set(
      Scenario.from(List(("a", "d"),("b", "e"),("c", "f"))),
      Scenario.from(List(("a", "d"),("b", "f"),("c", "e"))),
      Scenario.from(List(("a", "f"),("b", "d"),("c", "e"))),
      Scenario.from(List(("a", "f"),("b", "e"),("c", "d"))),
    )

    val expectedProbabilities = Set(
      CompleteProbabilityForWoman("a", Map("d" -> 0.5, "e" -> 0.00, "f" -> 0.5)),
      CompleteProbabilityForWoman("b", Map("d" -> 0.25, "e" -> 0.5, "f" -> 0.25)),
      CompleteProbabilityForWoman("c", Map("d" -> 0.25, "e" -> 0.5, "f" -> 0.25)))

    val season = threePairSeason.copy(scenarios = scenarios, noMatches = confirmedNoMatches, weekNumber = 1)
    StraightSeasonProbabilityCalculator.calculate(season) shouldBe expectedProbabilities
  }
}
