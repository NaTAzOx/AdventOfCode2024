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

    def displayRobots(robots: List[Robot]): Unit = {
        val grid = Array.fill(height, width)('.')
        robots.foreach { robot =>
            val (x, y) = robot.position
            grid(y)(x) = '#'
        }
        grid.foreach(row => println(row.mkString))
    }

    def findEasterEgg(robots: List[Robot]): Int = {
        val pattern = List(
            "        #        ",
            "       ###       ",
            "      #####      ",
            "     #######     ",
            "    #########    ",
            "      #####      ",
            "     #######     ",
            "    #########    ",
            "   ###########   ",
            "  #############  ",
            "     #######     ",
            "    #########    ",
            "   ###########   ",
            "  #############  ",
            " ############### ",
            "    #########    ",
            "   ###########   ",
            "  #############  ",
            " ############### ",
            "#################",
            "       ###       ",
            "       ###       ",
            "       ###       "
        )

        var seconds = 0
        var found = false

        while (!found) {
            val movedRobots = robots.map(moveRobot(_, seconds))
            println(s"Seconds: $seconds")
            displayRobots(movedRobots)

            val robotPositions = movedRobots.map(_.position).toSet
            val patternHeight = pattern.length
            val patternWidth = pattern.map(_.length).max

            found = (0 until (height - patternHeight)).exists { yOffset =>
                (0 until (width - patternWidth)).exists { xOffset =>
                    pattern.zipWithIndex.forall { case (row, y) =>
                        row.zipWithIndex.forall { case (char, x) =>
                            char != '#' || robotPositions.contains((x + xOffset, y + yOffset))
                        }
                    }
                }
            }

            if (!found) {
                seconds += 1
            }
        }

        seconds
    }

    def main(args: Array[String]): Unit = {
        val robots = parseInput("input.txt")
        val seconds = findEasterEgg(robots)
        println(s"Fewest number of seconds: $seconds")
    }
}