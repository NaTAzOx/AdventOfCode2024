import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.*;

public class December16_2 {
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

        @Override
        public String toString() {
            return "(" + x + ", " + y + ")";
        }
    }

    static class Node implements Comparable<Node> {
        Position position;
        int cost;
        String direction;

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
                    char c = lines.get(x).charAt(y);
                    if (c == 'S') {
                        start = new Position(x, y);
                    } else if (c == 'E') {
                        end = new Position(x, y);
                    } else if (c == '#') {
                        walls.add(new Position(x, y));
                    }
                }
            }

            System.out.println("Start: " + start);
            System.out.println("End: " + end);

            Map<Position, Set<Integer>> pathScores = findBestPathTiles(start, end, walls, lines);
            System.out.println("Number of tiles part of at least one best path: " + pathScores.size());

            System.out.println("Path scores:");
            for (Map.Entry<Position, Set<Integer>> entry : pathScores.entrySet()) {
                System.out.println("Position: " + entry.getKey() + " Scores: " + entry.getValue());
            }

        } catch (Exception e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    static Map<Position, Set<Integer>> findBestPathTiles(Position start, Position end, List<Position> walls, List<String> grid) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
        String[] directionNames = {"UP", "DOWN", "LEFT", "RIGHT"};

        PriorityQueue<Node> pq = new PriorityQueue<>();
        pq.add(new Node(start, 0, "EAST"));

        Map<Position, Integer> minCost = new HashMap<>();
        Map<Position, List<Position>> parents = new HashMap<>();
        Map<Position, Set<Integer>> pathScores = new HashMap<>();
        minCost.put(start, 0);
        pathScores.put(start, new HashSet<>(Collections.singletonList(0)));

        while (!pq.isEmpty()) {
            Node current = pq.poll();

            if (current.cost > minCost.getOrDefault(current.position, Integer.MAX_VALUE)) {
                continue;
            }

            for (int i = 0; i < directions.length; i++) {
                int newX = current.position.x + directions[i][0];
                int newY = current.position.y + directions[i][1];
                String newDirection = directionNames[i];
                Position neighbor = new Position(newX, newY);

                if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || walls.contains(neighbor)) {
                    continue;
                }

                int newCost = current.cost + 1;
                if (!current.direction.equals(newDirection)) {
                    newCost += 1000;
                }

                if (!minCost.containsKey(neighbor) || newCost < minCost.get(neighbor)) {
                    minCost.put(neighbor, newCost);
                    pq.add(new Node(neighbor, newCost, newDirection));
                    parents.put(neighbor, new ArrayList<>(List.of(current.position)));
                    pathScores.put(neighbor, new HashSet<>(Collections.singletonList(newCost)));
                } else if (newCost == minCost.get(neighbor)) {
                    parents.get(neighbor).add(current.position);
                    pathScores.get(neighbor).add(newCost);
                }
            }
        }

        Set<Position> bestPathTiles = new HashSet<>();
        Queue<Position> queue = new LinkedList<>();
        queue.add(end);
        while (!queue.isEmpty()) {
            Position current = queue.poll();
            if (bestPathTiles.contains(current)) {
                continue;
            }
            bestPathTiles.add(current);
            if (parents.containsKey(current)) {
                queue.addAll(parents.get(current));
            }
        }

        Map<Position, Set<Integer>> bestPathScores = new HashMap<>();
        for (Position tile : bestPathTiles) {
            bestPathScores.put(tile, pathScores.get(tile));
        }

        return bestPathScores;
    }
}
