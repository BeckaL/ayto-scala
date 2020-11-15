package display

import model.ProbabilityForWoman
import org.scalatest.{FlatSpec, Matchers}

class ProbabilityFormatterTest extends FlatSpec with Matchers {
  "format" should "format properly" in {
    val probabilities = Set(
      ProbabilityForWoman("a", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)),
      ProbabilityForWoman("b", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33)),
      ProbabilityForWoman("c", Map("d" -> 0.33, "e" -> 0.33, "f" -> 0.33))
    )
    val expectedResult =
        "    d      e      f     \n" +
        "a  0.33   0.33   0.33 \n" +
        "b  0.33   0.33   0.33 \n" +
        "c  0.33   0.33   0.33 "

    ProbabilityFormatter.format(probabilities) shouldBe expectedResult
  }

  "format" should "format properly with non 2 d.p numbers" in {
    val probabilities = Set(
      ProbabilityForWoman("a", Map("d" -> 1, "e" -> 0, "f" -> 0)),
      ProbabilityForWoman("b", Map("d" -> 0, "e" -> 0.5, "f" -> 0.5)),
      ProbabilityForWoman("c", Map("d" -> 0, "e" -> 0.5, "f" -> 0.5))
    )
    val expectedResult =
        "    d      e      f     \n" +
        "a  1.00   0.00   0.00 \n" +
        "b  0.00   0.50   0.50 \n" +
        "c  0.00   0.50   0.50 "

    ProbabilityFormatter.format(probabilities) shouldBe expectedResult
  }

  it should "round to 2 d.p." in {
    val probabilities = Set(ProbabilityForWoman("a", Map("d" -> 0, "e" -> 0.125, "f" -> 0.875)))

    val expectedResult =
        "    d      e      f     \n" +
        "a  0.00   0.13   0.88 "

    ProbabilityFormatter.format(probabilities) shouldBe expectedResult
  }

  it should "format properly with longer column headers" in {
    val probabilities = Set(
      ProbabilityForWoman("annabelle", Map("dan" -> 1, "ed" -> 0, "frederik" -> 0, "george" -> 0.0)),
      ProbabilityForWoman("becka", Map("dan" -> 0, "ed" -> 0.5, "frederik" -> 0.5, "george" -> 0.0)),
      ProbabilityForWoman("cat", Map("dan" -> 0, "ed" -> 0.5, "frederik" -> 0.5, "george" -> 0.0)),
    )

    val expectedResult =
        "           dan     ed    frederik   george   \n" +
        "annabelle  1.00   0.00     0.00      0.00  \n" +
        "becka      0.00   0.50     0.50      0.00  \n" +
        "cat        0.00   0.50     0.50      0.00  "

    ProbabilityFormatter.format(probabilities) shouldBe expectedResult
  }

}
