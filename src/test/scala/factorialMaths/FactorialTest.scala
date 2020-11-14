package factorialMaths

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FactorialTest extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  "Factorial" should "calculate a normal factorial" in {
    val data = Table(
      ("input", "expectedOutput"),
      (1, 1),
      (2, 2),
      (5, 120),
      (10, 3628800)
    )

    forAll(data) { case (input, expectedOutput) =>
      Factorial.factorial(input) shouldBe expectedOutput
    }
  }
}
