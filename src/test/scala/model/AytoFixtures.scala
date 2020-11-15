package model

trait AytoFixtures {
  val women = Set("a", "b", "c")
  val men = Set("d", "e", "f")
  val seasonName = "TestSeason"
  val threePairSeason = StraightSeason.from(seasonName, women, men)

  def pairsFrom(pairs: (String, String)*) = pairs.map(p => Pairing(p._1, p._2)).toSet

  val scenario1 = Scenario.from(List(("a", "d"), ("b", "e"), ("c", "f")))
  val scenario2 = Scenario.from(List(("a", "d"), ("b", "f"), ("c", "e")))
  val scenario3 = Scenario.from(List(("a", "e"), ("b", "d"), ("c", "f")))
  val scenario4 = Scenario.from(List(("a", "e"), ("b", "f"), ("c", "d")))
  val scenario5 = Scenario.from(List(("a", "f"), ("b", "d"), ("c", "e")))
  val scenario6 = Scenario.from(List(("a", "f"), ("b", "e"), ("c", "d")))

  val allScenarios = Set(scenario1, scenario2, scenario3, scenario4, scenario5, scenario6)
  val fourPairSeason = StraightSeason.from(seasonName, Set("a", "b", "c", "d"), Set("w", "x", "y", "z"))
}
