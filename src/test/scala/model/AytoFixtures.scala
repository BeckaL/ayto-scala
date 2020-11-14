package model

trait AytoFixtures {
  val women = Set("a", "b", "c")
  val men = Set("d", "e", "f")
  val seasonName = "TestSeason"
  val basicSeason = StraightSeason.from(seasonName, women, men)

  def pairsFrom(pairs: (String, String)*) = pairs.map(p => Pairing(p._1, p._2)).toSet

}
