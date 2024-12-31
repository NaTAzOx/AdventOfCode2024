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
            String path = "input.txt";
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

            List<List<Position>> allPaths = findAllBestPaths(start, end, walls, lines);

            // Afficher tous les meilleurs chemins
            System.out.println("Number of best paths: " + allPaths.size());
            for (List<Position> pathList : allPaths) {
                System.out.println(pathList);
            }

            // Visualiser les carreaux utilisés dans au moins un meilleur chemin
            Set<Position> bestPathTiles = new HashSet<>();
            for (List<Position> pathList : allPaths) {
                bestPathTiles.addAll(pathList);
            }

            for (int x = 0; x < lines.size(); x++) {
                for (int y = 0; y < lines.get(x).length(); y++) {
                    Position pos = new Position(x, y);
                    if (bestPathTiles.contains(pos)) {
                        System.out.print("O");
                    } else {
                        System.out.print(lines.get(x).charAt(y));
                    }
                }
                System.out.println();
            }

        } catch (Exception e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    static List<List<Position>> findAllBestPaths(Position start, Position end, List<Position> walls, List<String> grid) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        // Directions and their names
        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
        String[] directionNames = {"UP", "DOWN", "LEFT", "RIGHT"};

        // Priority queue for Dijkstra
        PriorityQueue<Node> pq = new PriorityQueue<>();
        pq.add(new Node(start, 0, "START"));

        // Maps to track costs and parent positions
        Map<Position, Integer> minCost = new HashMap<>();
        Map<Position, List<Position>> parents = new HashMap<>();
        minCost.put(start, 0);

        while (!pq.isEmpty()) {
            Node current = pq.poll();

            // If the current cost is higher than the recorded minimum cost, skip
            if (current.cost > minCost.getOrDefault(current.position, Integer.MAX_VALUE)) {
                continue;
            }

            // Explore neighbors
            for (int i = 0; i < directions.length; i++) {
                int newX = current.position.x + directions[i][0];
                int newY = current.position.y + directions[i][1];
                String newDirection = directionNames[i];
                Position neighbor = new Position(newX, newY);

                // Check boundaries and walls
                if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || walls.contains(neighbor)) {
                    continue;
                }

                // Calculate new cost
                int newCost = current.cost + 1; // Moving forward costs 1
                if (!current.direction.equals(newDirection)) {
                    newCost += 1000; // Changing direction costs 1000
                }

                // Update cost and parents if a better cost is found
                if (!minCost.containsKey(neighbor) || newCost < minCost.get(neighbor)) {
                    minCost.put(neighbor, newCost);
                    pq.add(new Node(neighbor, newCost, newDirection));
                    parents.put(neighbor, new ArrayList<>(List.of(current.position)));
                } else if (newCost == minCost.get(neighbor)) {
                    // If the cost is the same, add the parent to the list
                    parents.get(neighbor).add(current.position);
                }
            }
        }

        // Backtrack to find all paths
        List<List<Position>> allPaths = new ArrayList<>();
        backtrack(end, new ArrayList<>(), allPaths, parents);

        return allPaths;
    }

    static void backtrack(Position current, List<Position> path, List<List<Position>> allPaths, Map<Position, List<Position>> parents) {
        path.add(0, current); // Add current to the beginning of the path
        if (!parents.containsKey(current)) {
            allPaths.add(new ArrayList<>(path)); // Found a full path, add it to the results
        } else {
            for (Position parent : parents.get(current)) {
                backtrack(parent, path, allPaths, parents);
            }
        }
        path.remove(0); // Backtrack step
    }
}
