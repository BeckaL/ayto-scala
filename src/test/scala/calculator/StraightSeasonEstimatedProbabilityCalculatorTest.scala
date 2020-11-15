package calculator

import model.{AytoFixtures, ConfirmedInfo, IncompleteProbabilityRow, Pairing, StraightSeason}
import org.scalatest.{FlatSpec, Matchers}

class StraightSeasonEstimatedProbabilityCalculatorTest extends FlatSpec with Matchers with AytoFixtures{
  "EstimatedProbabilityCalculator" should "estimate probabilities when there are no confirmed matches or no matches" in {
    val expectedProbabilities = Set(
      IncompleteProbabilityRow("a", Map("w" -> Some(0.33), "x" -> Some(0.33), "y" -> Some(0.33))),
      IncompleteProbabilityRow("b", Map("w" -> Some(0.33), "x" -> Some(0.33), "y" -> Some(0.33))),
      IncompleteProbabilityRow("c", Map("w" -> Some(0.33), "x" -> Some(0.33), "y" -> Some(0.33))))

    StraightSeasonEstimatedProbabilityCalculator.calculate(threePairSeason) shouldBe expectedProbabilities
  }

  it should "estimate probabilities when there is some confirmed information" in {
    val women = threePairSeason.contestants.women + "d"
    val men = threePairSeason.contestants.men + "z"
    val perfectMatches = Set(Pairing("a", "x"))
    val noMatches = pairsFrom(("a", "y"), ("a", "w"), ("a", "z"), ("b", "x"), ("c", "x"), ("d", "x"), ("d", "w"))
    val season = StraightSeason
      .from("TestSeason2", women, men)
      .copy(confirmedInfo = ConfirmedInfo(noMatches = noMatches, perfectMatches = perfectMatches))

    val expectedProbabilities =  Set(
      IncompleteProbabilityRow("a", Map("w" -> Some(0.00), "x" -> Some(1.00), "y" -> Some(0.00), "z" -> Some(0.00))),
      IncompleteProbabilityRow("b", Map("w" -> None, "x" -> Some(0.00), "y" -> None, "z" -> None)),
      IncompleteProbabilityRow("c", Map("w" -> None, "x" -> Some(0.00), "y" -> None, "z" -> None)),
      IncompleteProbabilityRow("d", Map("w" -> Some(0.00), "x" -> Some(0.00), "y" -> None, "z" -> None)))

    StraightSeasonEstimatedProbabilityCalculator.calculate(season) shouldBe expectedProbabilities
  }

  override val threePairSeason = StraightSeason.from("testSeason", Set("a", "b", "c"), Set("w", "x", "y"))
}
