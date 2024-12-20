import java.util.*

class Position(val x: Int, val y: Int) : Comparable<Position> {
    operator fun plus(other: Position) = Position(x + other.x, y + other.y)

    override fun compareTo(other: Position): Int {
        if (x == other.x) {
            return y.compareTo(other.y)
        }
        return x.compareTo(other.x)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Position) return false
        return x == other.x && y == other.y
    }

    override fun hashCode(): Int {
        return 31 * x + y
    }

    override fun toString() = "($x, $y)"
}

class December18 {
    fun readInput(s: String, readLimit: Int): List<Position> {
        val lines = this::class.java.getResource(s)?.readText()?.trim()?.split("\n")
        val positions = mutableListOf<Position>()
        var count = 0
        if (lines != null) {
            for (pos in lines) {
                if (count++ > readLimit) {
                    break
                }
                val parts = pos.split(",").map { it.trim() }
                if (parts.size == 2) {
                    try {
                        val (x, y) = parts.map { it.toInt() }
                        positions.add(Position(x, y))
                    } catch (e: NumberFormatException) {
                        println("Skipping invalid line: $pos")
                    }
                } else {
                    println("Skipping invalid line: $pos")
                }
            }
        }
        return positions
    }

    fun initializeMap(width: Int, height: Int, walls: List<Position>): Map<Position, String> {
        val map = mutableMapOf<Position, String>()
        for (x in 0..width) {
            for (y in 0..height) {
                val pos = Position(x, y)
                if (walls.contains(pos)) {
                    map[pos] = "#"
                } else {
                    map[pos] = "."
                }
            }
        }
        return map
    }

    fun findShortestPath(map: Map<Position, String>, start: Position, end: Position): Int {
        val queue = PriorityQueue<Pair<Position, Int>>(compareBy { it.second })
        queue.add(Pair(start, 0))
        val visited = mutableSetOf<Position>()

        while (queue.isNotEmpty()) {
            val (current, distance) = queue.poll()
            if (current == end) {
                return distance
            }

            if (current in visited) {
                continue
            }

            visited.add(current)
            val neighbors = listOf(Position(0, 1), Position(0, -1), Position(1, 0), Position(-1, 0))

            for (neighbor in neighbors) {
                val next = current + neighbor
                if (map[next] == "." && next !in visited) {
                    queue.add(Pair(next, distance + 1))
                }
            }
        }
        return -1
    }
}

fun main() {
    val d = December18()
    val positions = d.readInput("input.txt", 1024)
    val map = d.initializeMap(70, 70, positions)

    for (y in 0..70) {
        for (x in 0..70) {
            print(map[Position(x, y)])
        }
        println()
    }

    val path = d.findShortestPath(map, Position(0, 0), Position(70, 70))
    println(path)
}