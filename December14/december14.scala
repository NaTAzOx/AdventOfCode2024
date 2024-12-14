import scala.io.Source

case class Robot(position: (Int, Int), velocity: (Int, Int))

object December14 {
    val width = 101
    val height = 103

    def parseInput(file: String): List[Robot] = {
        val lines = Source.fromFile(file).getLines().toList
        lines.map { line =>
            val parts = line.split(" ")
            val position = parts(0).substring(2).split(",").map(_.toInt)
            val velocity = parts(1).substring(2).split(",").map(_.toInt)
            Robot((position(0), position(1)), (velocity(0), velocity(1)))
        }
    }

    def moveRobot(robot: Robot, seconds: Int): Robot = {
        val newX = (robot.position._1 + robot.velocity._1 * seconds) % width
        val newY = (robot.position._2 + robot.velocity._2 * seconds) % height
        Robot((if (newX < 0) newX + width else newX, if (newY < 0) newY + height else newY), robot.velocity)
    }

    def countRobotsInQuadrants(robots: List[Robot]): (Int, Int, Int, Int) = {
        val midX = width / 2
        val midY = height / 2
        robots.foldLeft((0, 0, 0, 0)) { case ((q1, q2, q3, q4), robot) =>
            val (x, y) = robot.position
            if (x == midX || y == midY) (q1, q2, q3, q4)
            else if (x < midX && y < midY) (q1 + 1, q2, q3, q4)
            else if (x > midX && y < midY) (q1, q2 + 1, q3, q4)
            else if (x < midX && y > midY) (q1, q2, q3 + 1, q4)
            else (q1, q2, q3, q4 + 1)
        }
    }

    def main(args: Array[String]): Unit = {
        val robots = parseInput("December14/input.txt")
        val movedRobots = robots.map(moveRobot(_, 100))
        val (q1, q2, q3, q4) = countRobotsInQuadrants(movedRobots)
        val safetyFactor = q1 * q2 * q3 * q4
        println(safetyFactor)
    }
}