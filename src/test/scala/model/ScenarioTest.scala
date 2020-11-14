package model

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ScenarioTest extends FlatSpec with Matchers with AytoFixtures with TableDrivenPropertyChecks {
  "Scenario" should "correctly count the number of matching pairs" in {
    val data = Table(
      ("scenarioToCompare", "expectedNumberMatching"),
      (scenario1, 3),
      (scenario2, 1),
      (scenario3, 1),
      (scenario4, 0),
      (scenario5, 0),
      (scenario6, 1)
    )

    forAll(data) {case(scenarioToCompare, expectedNumberMatching) =>
      scenario1.numberMatching(scenarioToCompare) shouldBe expectedNumberMatching
    }
  }

}
