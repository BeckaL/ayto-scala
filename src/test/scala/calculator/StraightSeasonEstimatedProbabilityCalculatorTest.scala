package calculator

import model.{Pairing, StraightSeason, UncertainProbabilityForWoman}
import org.scalatest.{FlatSpec, Matchers}

class StraightSeasonEstimatedProbabilityCalculatorTest extends FlatSpec with Matchers {
  "EstimatedProbabilityCalculator" should "estimate probabilities when there are no confirmed matches or no matches" in {
    val expectedProbabilities = Set(
      UncertainProbabilityForWoman("a", Map("w" -> Some(0.33), "x" -> Some(0.33), "y" -> Some(0.33))),
      UncertainProbabilityForWoman("b", Map("w" -> Some(0.33), "x" -> Some(0.33), "y" -> Some(0.33))),
      UncertainProbabilityForWoman("c", Map("w" -> Some(0.33), "x" -> Some(0.33), "y" -> Some(0.33))))

    StraightSeasonEstimatedProbabilityCalculator.calculate(basicSeason) shouldBe expectedProbabilities
  }

  it should "estimate probabilities when there is some confirmed information" in {
    val women = basicSeason.contestants.women + "d"
    val men = basicSeason.contestants.men + "z"
    val perfectMatches = Set(Pairing("a", "x"))
    val noMatches = pairsFrom(Set(("a", "y"), ("a", "w"), ("a", "z"), ("b", "x"), ("c", "x"), ("d", "x"), ("d", "w")))
    val season = StraightSeason.from("TestSeason2", women, men).copy(noMatches = noMatches, perfectMatches = perfectMatches)

    val expectedProbabilities =  Set(
      UncertainProbabilityForWoman("a", Map("w" -> Some(0.00), "x" -> Some(1.00), "y" -> Some(0.00), "z" -> Some(0.00))),
      UncertainProbabilityForWoman("b", Map("w" -> None, "x" -> Some(0.00), "y" -> None, "z" -> None)),
      UncertainProbabilityForWoman("c", Map("w" -> None, "x" -> Some(0.00), "y" -> None, "z" -> None)),
      UncertainProbabilityForWoman("d", Map("w" -> Some(0.00), "x" -> Some(0.00), "y" -> None, "z" -> None)))

    StraightSeasonEstimatedProbabilityCalculator.calculate(season) shouldBe expectedProbabilities
  }

  private val basicSeason = StraightSeason.from("testSeason", women, men)
  private lazy val women = Set("a", "b", "c")
  private lazy val men = Set("w", "x", "y")

  private def pairsFrom(pairs: Set[(String, String)]) = pairs.map(p => Pairing(p._1, p._2))

}
