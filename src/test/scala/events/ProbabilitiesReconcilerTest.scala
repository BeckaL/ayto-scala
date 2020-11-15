package events

import model.{AytoFixtures, CompleteProbabilityRow, ProbabilityResult, IncompleteProbabilityRow}
import org.scalatest.{FlatSpec, Matchers}

class ProbabilitiesReconcilerTest extends FlatSpec with Matchers with AytoFixtures {
  "Probabilities reconciler" should "get no matches and matches from probability table" in {
    val probabilities = Set(
      CompleteProbabilityRow("a", Map("d" -> 1, "e" -> 0, "f" -> 0)),
      CompleteProbabilityRow("b", Map("d" -> 0, "e" -> 0.5, "f" -> 0.5)),
      CompleteProbabilityRow("c", Map("d" -> 0, "e" -> 0.5, "f" -> 0.5))
    )

    val updatedSeason = ProbabilitiesReconciler.reconcileComplete(probabilities,threePairSeason)

    updatedSeason.perfectMatches shouldBe pairsFrom(("a", "d"))
    updatedSeason.noMatches  shouldBe pairsFrom(("b", "d"), ("c", "d"), ("a", "e"), ("a", "f"))
  }

  it should "get no matches and matches from uncertain probability table" in {
    val probabilities = Set(
      IncompleteProbabilityRow("a", Map("d" -> Some(1.0), "e" -> Some(0.0), "f" -> Some(0.0))),
      IncompleteProbabilityRow("b", Map("d" -> Some(0.0), "e" -> None, "f" -> None)),
      IncompleteProbabilityRow("c", Map("d" -> Some(0.0), "e" -> None, "f" -> None))
    )

    val updatedSeason = ProbabilitiesReconciler.reconcileIncomplete(probabilities, threePairSeason)

    updatedSeason.perfectMatches shouldBe pairsFrom(("a", "d"))
    updatedSeason.noMatches  shouldBe pairsFrom(("b", "d"), ("c", "d"), ("a", "e"), ("a", "f"))
  }


}
