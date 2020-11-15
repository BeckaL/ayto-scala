package display

import model.{CompleteProbabilityForWoman, IncompleteProbabilityForWoman}

object ProbabilityFormatter {
  def formatComplete(probabilities: Set[CompleteProbabilityForWoman]): String = {
    val men = probabilities.head.probabilitiesForMen.keys.toList.sorted
    val longestRowHeader = probabilities.map(_.woman).toList.map(_.length).max
    val topRow = " " * longestRowHeader + "  " + men.map(padColumnHeader).mkString
    val otherRows = probabilities.map(p =>
      padRowHeader(p.woman, longestRowHeader)
        + "  "
        + men.map(m => padDigit(p.probabilitiesForMen(m), m.length)).mkString("  ")
    )
    topRow + "\n" + otherRows.mkString("\n")
  }

  def formatIncomplete(probabilities: Set[IncompleteProbabilityForWoman]): String = {
    val men = probabilities.head.probabilitiesForMen.keys.toList.sorted
    val longestRowHeader = probabilities.map(_.woman).toList.map(_.length).max
    val topRow = " " * longestRowHeader + "  " + men.map(padColumnHeader).mkString
    val otherRows = probabilities.map(p =>
      padRowHeader(p.woman, longestRowHeader)
        + "  "
        + men.map(m => padDigit(p.probabilitiesForMen(m), m.length)).mkString("  ")
    )
    topRow + "\n" + otherRows.mkString("\n")
  }

  private def padColumnHeader(name: String) =
    padWith(name, (4 - name.length)/2.0) +  "   "

  private def padDigit(n: Double, nameLength: Int): String =
    padWith(f"$n%2.2f" + " ", (nameLength - 4)/ 2.0)

  private def padDigit(n: Option[Double], nameLength: Int): String = n match {
      case None => padWith("???? ", (nameLength - 4)/ 2.0)
      case Some(i) => padDigit(i, nameLength)
    }

  private def padRowHeader(name: String, maxSize: Int): String = name + " " * (maxSize - name.length)

  private def padWith(s: String, lToPadOnEachSide: Double) =
    " " * lToPadOnEachSide.floor.toInt + s + " " * lToPadOnEachSide.ceil.toInt
}
