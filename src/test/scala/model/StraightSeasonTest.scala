package model

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class StraightSeasonTest extends FlatSpec with Matchers with AytoFixtures with TableDrivenPropertyChecks {
  "Straight season" should "be created correctly from a list of men and women" in {
    val expectedPossiblePairings = Set(
      Pairing("a", "d"), Pairing("a", "e"), Pairing("a", "f"),
      Pairing("b", "d"), Pairing("b", "e"), Pairing("b", "f"),
      Pairing("c", "d"), Pairing("c", "e"), Pairing("c", "f")
    )

    threePairSeason shouldBe StraightSeason(seasonName, Contestants(women, men), 0, allScenarios, expectedPossiblePairings, Set.empty, Set.empty)
    threePairSeason.initialNumberOfProbabilities shouldBe 6
  }

  it should "be able to say whether or not it has any confirmed information" in {
    val information = pairsFrom(("a", "d"))
    val data = Table(
      ("season", "expectedHasNoConfirmedInformation"),
      (threePairSeason.copy(noMatches =  information), false),
      (threePairSeason.copy(perfectMatches =  information), false),
      (threePairSeason.copy(noMatches =  information, perfectMatches = pairsFrom(("b", "d"))), false),
      (threePairSeason, true)
    )

    forAll(data) { case(season, expectedHasNoConfirmedInformation) =>
      season.hasNoConfirmedInformation shouldBe expectedHasNoConfirmedInformation
    }

  }
}



