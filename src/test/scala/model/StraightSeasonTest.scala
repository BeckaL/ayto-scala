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

    threePairSeason shouldBe StraightSeason(seasonName, Contestants(women, men), 0, allScenarios, expectedPossiblePairings, ConfirmedInfo(Set.empty, Set.empty))
    threePairSeason.initialNumberOfProbabilities shouldBe 6
  }

  it should "be able to say whether or not it has any confirmed information" in {
    val information = pairsFrom(("a", "d"))
    val data = Table(
      ("season", "expectedHasNoConfirmedInformation"),
      (threePairSeason.copy(confirmedInfo = ConfirmedInfo(Set.empty, noMatches =  information)), false),
      (threePairSeason.copy(confirmedInfo = ConfirmedInfo(perfectMatches =  information, Set.empty)), false),
      (threePairSeason.copy(confirmedInfo = ConfirmedInfo(noMatches =  information, perfectMatches = pairsFrom(("b", "d")))), false),
      (threePairSeason, true)
    )

    forAll(data) { case(season, expectedHasNoConfirmedInformation) =>
      season.hasNoConfirmedInformation shouldBe expectedHasNoConfirmedInformation
    }
  }

  it should "be able to say whether or not it is solved" in {
    val data = Table(
      ("season", "expectedHasNoConfirmedInformation"),
      (threePairSeason.copy(confirmedInfo = ConfirmedInfo(perfectMatches = Set(), Set())), false),
      (threePairSeason.copy(confirmedInfo = ConfirmedInfo(perfectMatches = scenario2.pairs, Set())), true),
      (threePairSeason.copy(confirmedInfo = ConfirmedInfo(perfectMatches = scenario2.pairs.take(2), Set())), false)
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
      (ConfirmedInfo(perfectMatches = Set.empty, noMatches = emptyInfo), (emptyInfo, noMatchInfo1), (emptyInfo, noMatchInfo1)),
      (ConfirmedInfo(perfectMatches = Set.empty, noMatches = noMatchInfo1), (emptyInfo, noMatchInfo2), (emptyInfo, noMatchInfo3)),
      (ConfirmedInfo(perfectMatches = Set.empty, noMatches = noMatchInfo3), (emptyInfo, noMatchInfo3), (emptyInfo, noMatchInfo3)),
      (ConfirmedInfo(perfectMatches = emptyInfo, noMatches = Set.empty), (perfectMatchInfo1, emptyInfo), (perfectMatchInfo1, emptyInfo)),
      (ConfirmedInfo(perfectMatches = perfectMatchInfo1, noMatches = Set.empty), (perfectMatchInfo2, emptyInfo), (perfectMatchInfo3, emptyInfo)),
      (ConfirmedInfo(perfectMatches = perfectMatchInfo3, noMatches = Set.empty), (perfectMatchInfo3, emptyInfo), (perfectMatchInfo3, emptyInfo)),
    )

    forAll(data) { case(confirmedInfo, newInfo, expectedInfo) =>
      val season = threePairSeason.copy(confirmedInfo = confirmedInfo)
      val updatedSeason = season.updateWithInfo(ConfirmedInfo(newInfo._1, newInfo._2))
      updatedSeason.name shouldBe season.name
      updatedSeason.contestants shouldBe season.contestants
      updatedSeason.possiblePairings shouldBe season.possiblePairings
      updatedSeason.scenarios shouldBe season.scenarios
      updatedSeason.confirmedInfo shouldBe ConfirmedInfo(expectedInfo._1, expectedInfo._2)
    }
  }
}



