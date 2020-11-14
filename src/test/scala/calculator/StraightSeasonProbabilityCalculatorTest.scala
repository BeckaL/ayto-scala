package calculator

import model.{Pairing, ProbabilityForWoman, Scenario, StraightSeason}
import org.scalatest.{FlatSpec, Matchers}

class StraightSeasonProbabilityCalculatorTest extends FlatSpec with Matchers {
  "StraightSeasonProbabilityCalculator" should "calculate probabilities from existing confirmed matches and no matches" in {
    val confirmedMatches = pairsFrom(Set(("a", "d"), ("b", "e"), ("c", "f")))
    val confirmedNoMatches = pairsFrom(Set(("a", "e"), ("a", "f"), ("b", "d"), ("b", "f"), ("c", "d"), ("c", "e")))
    val season = basicSeason.copy(perfectMatches = confirmedMatches, noMatches = confirmedNoMatches)

    val expectedProbabilities = Set(
      ProbabilityForWoman("a", Map("d" -> 1.00, "e" -> 0.00, "f" -> 0.00)),
      ProbabilityForWoman("b", Map("d" -> 0.00, "e" -> 1.00, "f" -> 0.00)),
      ProbabilityForWoman("c", Map("d" -> 0.00, "e" -> 0.00, "f" -> 1.00)))

    StraightSeasonProbabilityCalculator.calculate(season) shouldBe expectedProbabilities
  }

  it should "calculate probabilities when the week number is 0 and there is no confirmed information" in {
    val expectedProbabilities = Set(
      ProbabilityForWoman("a", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)),
      ProbabilityForWoman("b", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)),
      ProbabilityForWoman("c", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)))

    StraightSeasonProbabilityCalculator.calculate(basicSeason) shouldBe expectedProbabilities
  }


  it should "calculate probabilities when there is a mix of confirmed information and non confirmed information" in {
    val confirmedNoMatches = pairsFrom(Set(("a", "e")))
    val scenarios = Set(
      Scenario.from(List(("a", "d"),("b", "e"),("c", "f"))),
      Scenario.from(List(("a", "d"),("b", "f"),("c", "e"))),
      Scenario.from(List(("a", "f"),("b", "d"),("c", "e"))),
      Scenario.from(List(("a", "f"),("b", "e"),("c", "d"))),
    )

    val expectedProbabilities = Set(
      ProbabilityForWoman("a", Map("d" -> 0.5, "e" -> 0.00, "f" -> 0.5)),
      ProbabilityForWoman("b", Map("d" -> 0.25, "e" -> 0.5, "f" -> 0.25)),
      ProbabilityForWoman("c", Map("d" -> 0.25, "e" -> 0.5, "f" -> 0.25)))

    val season = basicSeason.copy(scenarios = scenarios, noMatches = confirmedNoMatches, weekNumber = 1)
    StraightSeasonProbabilityCalculator.calculate(season) shouldBe expectedProbabilities
  }

  private def pairsFrom(pairs: Set[(String, String)]) = pairs.map(p => Pairing(p._1, p._2))

  private val basicSeason = StraightSeason.from("testSeason", women, men)
  private lazy val women = Set("a", "b", "c")
  private lazy val men = Set("d", "e", "f")
}
