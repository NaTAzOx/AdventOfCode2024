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
    fun readInput(s: String): List<Position> {
        val lines = this::class.java.getResource(s)?.readText()?.trim()?.split("\n")
        val positions = mutableListOf<Position>()
        if (lines != null) {
            for (pos in lines) {
                val (x, y) = pos.split(",").map { it.toInt() }
                positions.add(Position(x, y))
            }
        }
        return positions
    }

    fun initializeMap(width: Int, height: Int, walls: List<Position>): MutableMap<Position, String> {
        val map = mutableMapOf<Position, String>()
        for (x in 0..width) {
            for (y in 0..height) {
                val pos = Position(x, y)
                if (walls.find { it.x == x && it.y == y } != null) {
                    map[pos] = "#"
                } else {
                    map[pos] = "."
                }
            }
        }
        return map
    }

    fun findShortestPath(map: MutableMap<Position, String>, start: Position, end: Position): Int {
        val directions = listOf(Position(0, 1), Position(0, -1), Position(1, 0), Position(-1, 0))
        val distances = mutableMapOf<Position, Int>().withDefault { Int.MAX_VALUE }
        val queue = PriorityQueue(compareBy<Pair<Position, Int>> { it.second })

        distances[start] = 0
        queue.add(start to 0)

        while (queue.isNotEmpty()) {
            val (current, currentDistance) = queue.poll()

            if (current == end) return currentDistance

            for (direction in directions) {
                val neighbor = current + direction
                if (map[neighbor] == "#") continue

                val newDistance = currentDistance + 1
                if (newDistance < distances.getValue(neighbor)) {
                    distances[neighbor] = newDistance
                    queue.add(neighbor to newDistance)
                }
            }
        }

        return -1 // Return -1 if there is no path to the end position
    }
}

fun main() {
    val d = December18()
    val positions = d.readInput("input")
    val map = d.initializeMap(70, 70, positions)
    println(map)
    println(map[Position(0, 0)])
    val path = d.findShortestPath(map, Position(0, 0), Position(70, 70))
    println(path)
}