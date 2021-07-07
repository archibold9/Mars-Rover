package Rover

import Driver.{Coordinate, Navigation, Rover, RoverDriver, Terrain}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RoverTest extends AnyWordSpec with Matchers {

  "Rover" should {
    "have accessible coordinates and an orientation" in {
      val rover = Rover(0, Coordinate(0,0))
      rover.coordinates shouldBe Coordinate(0,0)
      rover.orientation shouldBe 0
    }
  }

  "Rotating clockwise" should {
    "Cause the orientation to change" in {
      val rover = Rover(0, Coordinate(0,0))
      val rotatedRover = RoverDriver.rotateClockwise(rover)
      rotatedRover.orientation shouldBe 1
    }
  }

  "Rotating anticlockwise" should {
    "Cause the orientation to change" in {
      val rover = Rover(0, Coordinate(0,0))
      val rotatedRover = RoverDriver.rotateAntiClockwise(rover)
      rotatedRover.orientation shouldBe 3
    }
  }

  "Rotating clockwise 4 times" should {
    "Cause the orientation to remain the same" in {
      val rover = Rover(0, Coordinate(0,0))
      RoverDriver.rotateClockwise(RoverDriver.rotateClockwise(RoverDriver.rotateClockwise(RoverDriver.rotateClockwise(rover)))).orientation shouldBe 0
    }
  }

  "finding a path" should {
    "succeed if the constraints are in bounds" in {
      val rover = Rover(0, Coordinate(0,0))
      val terrain = Terrain(10,10)

      Navigation.findPath(rover, Coordinate(5,5), terrain) should not be empty
    }
  }

  "finding a path" should {
    "throw IllegalArgumentException if the constraints are not in bounds" in {
      val rover = Rover(0, Coordinate(0,0))
      val terrain = Terrain(10,10)
      assertThrows[IllegalArgumentException] { // Result type: Assertion
        Navigation.findPath(rover, Coordinate(20,5), terrain)
      }
    }
  }

}