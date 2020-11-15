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

  it should "be able to say whether or not it is solved" in {
    val data = Table(
      ("season", "expectedHasNoConfirmedInformation"),
      (threePairSeason.copy(perfectMatches = Set()), false),
      (threePairSeason.copy(perfectMatches = scenario2.pairs), true),
      (threePairSeason.copy(perfectMatches = scenario2.pairs.take(2)), false)
    )

    forAll(data) { case(season, expectedIsSolved) =>
      season.isSolved shouldBe expectedIsSolved
    }
  }

  it should "add matches and no matches to existing information" in {
    val noMatchInfo1 = pairsFrom(("a", "d"))
    val noMatchInfo2 = pairsFrom(("a", "e"), ("b", "d"))
    val noMatchInfo3 = pairsFrom(("a", "d"), ("a", "e"), ("b", "d"))

    val perfectMatchInfo1 = pairsFrom(("a", "d"))
    val perfectMatchInfo2 = pairsFrom(("b", "d"))
    val perfectMatchInfo3 = pairsFrom(("a", "d"), ("b", "d"))

    val emptyInfo = Set[Pairing]()

    val data = Table(
      ("season", "newInfo", "expectedInfo"),
      (threePairSeason.copy(noMatches = emptyInfo), (emptyInfo, noMatchInfo1), (emptyInfo, noMatchInfo1)),
      (threePairSeason.copy(noMatches = noMatchInfo1), (emptyInfo, noMatchInfo2), (emptyInfo, noMatchInfo3)),
      (threePairSeason.copy(noMatches = noMatchInfo3), (emptyInfo, noMatchInfo3), (emptyInfo, noMatchInfo3)),
      (threePairSeason.copy(perfectMatches = emptyInfo), (perfectMatchInfo1, emptyInfo), (perfectMatchInfo1, emptyInfo)),
      (threePairSeason.copy(perfectMatches = perfectMatchInfo1), (perfectMatchInfo2, emptyInfo), (perfectMatchInfo3, emptyInfo)),
      (threePairSeason.copy(perfectMatches = perfectMatchInfo3), (perfectMatchInfo3, emptyInfo), (perfectMatchInfo3, emptyInfo)),
    )

    forAll(data) { case(season, newInfo, expectedInfo) =>
      val updatedSeason = season.updateWithInfo(newInfo._1, newInfo._2)
      updatedSeason.name shouldBe season.name
      updatedSeason.contestants shouldBe season.contestants
      updatedSeason.possiblePairings shouldBe season.possiblePairings
      updatedSeason.scenarios shouldBe season.scenarios
      updatedSeason.perfectMatches shouldBe expectedInfo._1
      updatedSeason.noMatches shouldBe expectedInfo._2
    }
  }
}



