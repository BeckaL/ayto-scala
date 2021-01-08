package model

import java.nio.file.{Files, Path}

import factorialMaths.Factorial

import scala.io.Source
import scala.reflect.io.{File, Path}

case class FromFileStraightSeason(name: String,
                                   contestants: Contestants,
                                   weekNumber: Int,
                                   scenarios: PathToFile,
                                   possiblePairings: Set[Pairing],
                                   confirmedInfo: ConfirmedInfo
                                 ) extends StraightSeason[PathToFile] {

  val initialNumberOfProbabilities: Int = Factorial.factorial(contestants.men.size)

  val hasNoConfirmedInformation = confirmedInfo.perfectMatches.isEmpty && confirmedInfo.noMatches.isEmpty
  val isSolved = confirmedInfo.perfectMatches.size == contestants.women.size

  def updateWithInfo(newConfirmedInfo: ConfirmedInfo): FromFileStraightSeason =
    this.copy(
      confirmedInfo = ConfirmedInfo(
        perfectMatches = this.confirmedInfo.perfectMatches | newConfirmedInfo.perfectMatches,
        noMatches = this.confirmedInfo.noMatches | newConfirmedInfo.noMatches
      )
    )
}

object FromFileStraightSeason {
  def from(seasonName: String, women: Set[String], men: Set[String]): InMemoryStraightSeason = {
    val possible_pairings = create_possible_pairings(women, men)
    val initial_scenarios = save_initial_scenarios(women, men, seasonName)
    val contestants = Contestants(women, men)
    FromFileStraightSeason(seasonName, contestants, 0, initial_scenarios, possible_pairings, ConfirmedInfo(Set.empty, Set.empty))
  }

  private def create_possible_pairings(women: Set[String], men: Set[String]): Set[Pairing] =
    for {
      woman <- women
      man <- men
    } yield Pairing(woman, man)

  private def save_initial_scenarios(women: Set[String], men: Set[String], seasonName: String): PathToFile =
    val pathToFile = Files.createDirectory(Path)
    men.toList.permutations.map(women.toList.zip(_)).foreach((pairings: Seq[(String, String)]) => )
}

final case class PathToFile(path: String)
