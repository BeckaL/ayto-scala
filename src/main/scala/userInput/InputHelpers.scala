package userInput

import model.{Pairing, InMemoryStraightSeason}

import scala.util.{Success, Try}

trait InputHelpers {
  val inOut: InputOutput
  type NamesToIndex = Set[(String, Int)]

  protected def namesWithIndexes(season: InMemoryStraightSeason): (NamesToIndex, NamesToIndex) = {
    val womenWithIndex: Set[(String, Int)] = season.contestants.women.zipWithIndex
    val menWithIndex = season.contestants.men.zipWithIndex.map{case(name, i) => (name, i + womenWithIndex.size)}
    (womenWithIndex, menWithIndex)
  }

  protected def getCouple(womenWithIndex: NamesToIndex, menWithIndex: NamesToIndex): Pairing = {
    val input = inOut.getInput(displayRemaining(womenWithIndex, menWithIndex) + "\nEnter two numbers, separated by a space")
    input.split(" ").map(i => Try(i.toInt)) match {
      case Array(Success(i1), Success(i2)) => getCoupleIfValid(i1, i2, womenWithIndex, menWithIndex)
      case _ =>
        inOut.print("Error! That didn't look right - you must input two numbers separated by a space. Please pick again")
        getCouple(womenWithIndex, menWithIndex)
    }
  }

  private def getCoupleIfValid(i1: Int, i2: Int, womenWithIndex: NamesToIndex, menWithIndex: NamesToIndex): Pairing = {
    val woman = womenWithIndex.find{case(_, i) => i == i1 | i == i2}
    val man = menWithIndex.find{case (_, i) => i == i1 | i == i2}
    (woman, man) match {
      case (Some(w), Some(m)) if w._1 != "" & m._1 != "" => Pairing(w._1, m._1)
      case _ =>
        inOut.print("Error! That didn't look like a valid couple to me. Please pick again")
        getCouple(womenWithIndex, menWithIndex)
    }
  }

  private def displayRemaining(womenWithIndex: NamesToIndex, menWithIndex: NamesToIndex): String = {
    val maxWomensNameLength = womenWithIndex.map(_._1.length).max
    sorted(womenWithIndex).zip(menWithIndex.toList).map{ case (womanAndIndex, manAndIndex) =>
      val padding = " " * (maxWomensNameLength - womanAndIndex._1.length) + "   "
      s"${formatNameAndIndex(womanAndIndex)} $padding ${formatNameAndIndex(manAndIndex)}"
    }.mkString("\n")
  }

  private def sorted(namesToIndex: NamesToIndex): List[(String, Int)] = namesToIndex.toList.sortBy(_._2)

  private def formatNameAndIndex(nameAndIndex: (String, Int)): String = s"${nameAndIndex._2}. ${nameAndIndex._1}"

}
