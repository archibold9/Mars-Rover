package Driver

object RoverDriver {

  def getAccessibleNeighbouringCells(rover: Rover, terrain: Terrain): Map[Int, Coordinate] = {
    val width = terrain.maxRow
    val height = terrain.maxCol
    // We use the mod here to wrap around the grid if necessary, meaning we are presented with
    // all possible directions (off the edges of the grid), so long as they are none mountainous

    // east, west = (x', y') = (x, (y +- 1 % matrix.length))
    // north, south = (x', y') = ((x +- 1 % matrix.length), y)
    Map(
      0 -> Coordinate(rover.coordinates.row, Math.floorMod(rover.coordinates.col + 1, height)), // N
      1 -> Coordinate(Math.floorMod(rover.coordinates.row + 1, width), rover.coordinates.col), // E
      2 -> Coordinate(rover.coordinates.row, Math.floorMod(rover.coordinates.col - 1, height)), // S
      3 -> Coordinate(Math.floorMod(rover.coordinates.row - 1, width), rover.coordinates.col) // W
    )
  }

  // If we consider due north to be orientation 0 degrees and use that as the true point, we don't need to maintain
  // different values and we can simply just mod by 4 (for each side of the grid) to achieve a single pivot point

  def rotateClockwise(rover: Rover): Rover =
    rover.copy(orientation = Math.floorMod(rover.orientation + 1, 4))

  def rotateAntiClockwise(rover: Rover): Rover = {
    rover.copy(orientation = Math.floorMod(rover.orientation - 1, 4))
  }

  def moveForward(rover: Rover, terrain: Terrain): Rover = {
    // We want to move towards our orientation, so we can just match on the map values returned from getAccessibleNeighbouringCells,
    // since they directly correlate and this function doesn't need any knowledge of the wrap around
    rover.copy(coordinates = getAccessibleNeighbouringCells(rover, terrain)(rover.orientation))
  }
}

object Navigation {
  private def getVectorDistance(start: Coordinate, end: Coordinate): Int = {
    // Essentially provides you with the gradient, or how many perpendicular movements the rover needs to make to reach
    // its destination coordinates
    Math.abs(start.row - end.row) + Math.abs(start.col - end.col)
  }

  private def getVelocityVector(start: Coordinate, end: Coordinate): Double = {
    Math.atan2(end.col - start.col, end.row - start.row)
  }

  // Use the vector distance as a minimisation function. This would work
  // with mountains, if we simply extended getAccessibleNeighbouringCells to take a filter function for available
  // coordinates. This method is also optimised for wrapping around the grid if the coordinate is closer to us via wrapping
  def findPath(rover: Rover, end: Coordinate, terrain: Terrain): Seq[Coordinate] = {
    if(end.row > terrain.maxRow || end.col > terrain.maxCol) {
      throw new IllegalArgumentException("Coordinate does not exist on the plane")
    }
    def recurse(rover: Rover, seqAcc: Seq[Coordinate]): Seq[Coordinate] = {
      if (rover.coordinates == end) {
        seqAcc
      } else {
        val possibleMovements = RoverDriver.getAccessibleNeighbouringCells(rover, terrain).values.toList
        // this will return a list of the values of all adjacent coordinates
        // we want to move to the one which reduces the total distance left, so map over all of them
        val weights: List[Int] = possibleMovements.map(c => getVectorDistance(c, end))
        val bestIndex = weights.zipWithIndex.min._2 // Find the index of the best coordinate to move to
        // once we know which is the best one, we add it to the sequence and update our current coordinates
        // Once we have this
        recurse(rover.copy(coordinates = possibleMovements(bestIndex)), seqAcc ++ Seq(possibleMovements(bestIndex)))
      }
    }
    recurse(rover, Seq())
  }

  def drivePathAndLog(rover: Rover, target: Coordinate, terrain: Terrain): Rover = {
    val sequenceToDrive = findPath(rover, target, terrain)
    // Once we have the sequence, just orient ourselves towards the next coordinate in the sequence and moveForward() while logging to
    // text or JSON or something graphical
    ???
  }
}

case class Coordinate(row: Int, col: Int)

case class Rover(orientation: Int, coordinates: Coordinate) {
  val ORIENTATION_TRANSCRIPTION = Map(0 -> "DUE NORTH", 1 -> "DUE EAST", 2 -> "DUE SOUTH", 3 -> "DUE WEST")

  override def toString: String = {
    val textOrientation = ORIENTATION_TRANSCRIPTION(orientation)
    s"Orientation => $textOrientation\nCoordinates => $coordinates"
  }
}

case class Terrain(maxRow: Int, maxCol: Int)

object Main extends App {
  val rover = Rover(0, Coordinate(0,0))
  println(rover)

  println("Successfully calculated path: " + Navigation.findPath(rover, Coordinate(5,5), Terrain(10, 10)))
}
