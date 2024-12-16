import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.*;

public class December16 {
    static class Position {
        int x, y;

        Position(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Position position = (Position) o;
            return x == position.x && y == position.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    static class Node implements Comparable<Node> {
        Position position;
        int cost;
        String direction; // "UP", "DOWN", "LEFT", "RIGHT"

        Node(Position position, int cost, String direction) {
            this.position = position;
            this.cost = cost;
            this.direction = direction;
        }

        @Override
        public int compareTo(Node other) {
            return Integer.compare(this.cost, other.cost);
        }
    }

    public static void main(String[] args) {
        try {
            List<Position> walls = new ArrayList<>();
            Position start = null, end = null;
            String path = "December16/input.txt";
            List<String> lines = new ArrayList<>();
            File file = new File(path);
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
            reader.close();

            for (int x = 0; x < lines.size(); x++) {
                for (int y = 0; y < lines.get(x).length(); y++) {
                    if (lines.get(x).charAt(y) == 'S') {
                        start = new Position(x, y);
                    } else if (lines.get(x).charAt(y) == 'E') {
                        end = new Position(x, y);
                    } else if (lines.get(x).charAt(y) == '#') {
                        walls.add(new Position(x, y));
                    }
                }
            }

            int shortestPathCost = dijkstra(start, end, walls, lines);
            System.out.println("Shortest path cost: " + shortestPathCost);

        } catch (Exception e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    static int dijkstra(Position start, Position end, List<Position> walls, List<String> grid) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
        String[] directionNames = {"UP", "DOWN", "LEFT", "RIGHT"};

        PriorityQueue<Node> pq = new PriorityQueue<>();
        pq.add(new Node(start, 0, ""));

        Map<String, Integer> visited = new HashMap<>();

        while (!pq.isEmpty()) {
            Node current = pq.poll();
            String key = current.position.x + "," + current.position.y + "," + current.direction;

            if (visited.containsKey(key) && visited.get(key) <= current.cost) {
                continue;
            }
            visited.put(key, current.cost);

            if (current.position.equals(end)) {
                return current.cost;
            }

            for (int i = 0; i < directions.length; i++) {
                int newX = current.position.x + directions[i][0];
                int newY = current.position.y + directions[i][1];
                String newDirection = directionNames[i];

                if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || walls.contains(new Position(newX, newY))) {
                    continue;
                }

                int newCost = current.cost + 1;
                if (!current.direction.equals("") && !current.direction.equals(newDirection)) {
                    newCost += 1000;
                }

                pq.add(new Node(new Position(newX, newY), newCost, newDirection));
            }
        }

        return -1;
    }
}
