import gleam/io
import gleam/list
import gleam/string

type Position {
  Pos(Int, Int)
}

type Map {
  Map(Position, String)
}

// Parse the input string into a map
fn parse_map(input: String) -> Map {
  input
  |> string.split("\n")
  |> list.indexed_map(fn(row_index, row) {
    row
    |> string.to_graphemes()
    |> list.indexed_map(fn(col_index, c) { tuple(Pos(row_index, col_index), c) })
  })
  |> list.concat
  |> map.from_list
}

// Directions for neighbors (right, down, left, up)
const directions = [Pos(0, 1), Pos(1, 0), Pos(0, -1), Pos(-1, 0)]

// Check if a position is within bounds
fn in_bounds(pos: Position, map: Map) -> Bool {
  map |> map.keys |> list.member(pos)
}

// Explore a region in the map
fn explore_region(
  map: Map,
  visited: Map,
  pos: Position,
  char: String,
) -> #(Int, Int, Map) {
  let area = 0
  let perimeter = 0
  let stack = [pos]
  let new_visited = visited

  while
  list.is_empty(stack) == False
  {
    let [current, rest] = stack
    let stack = rest

    case map.get(current) {
      Some(c) if c == char -> {
        case new_visited.get(current) {
          None -> {
            let area = area + 1
            let new_visited = map.insert(current, True, new_visited)

            for
            direction
            in
            directions
            {
              let Pos(dx, dy) = direction
              let Pos(x, y) = current
              let neighbor = Pos(x + dx, y + dy)

              let #(new_stack, new_perimeter) = case in_bounds(neighbor, map) {
                True -> {
                  case map.get(neighbor) {
                    Some(nc) if nc == char -> {
                      case new_visited.get(neighbor) {
                        None -> #([neighbor, stack], perimeter)
                        Some(_) -> #(stack, perimeter + 1)
                      }
                    }
                    _ -> {
                      stack
                      perimeter + 1
                    }
                  }
                }
                False -> {
                  stack
                  perimeter + 1
                }
              }
              let stack = new_stack
              let perimeter = new_perimeter
            }
          }
          Some(_) -> []
        }
      }
      _ -> []
    }
  }

  tuple(area, perimeter, new_visited)
}

// Calculate the total cost of the map
fn calculate_cost(map: Map) -> Int {
  let visited = map.new()
  let total_cost = 0

  for
  {
    let key = pos
    let value = char
  }
  in
  map
  {
    case visited.get(pos) {
      None -> {
        let #(area, perimeter, new_visited) =
          explore_region(map, visited, pos, char)
        let visited = new_visited
        let total_cost = total_cost + { area * perimeter }
      }
      Some(_) -> total_cost
    }
  }

  total_cost
}

// Example usage
pub fn main() {
  let filename = "input.txt"
  let input = io.read_file(filename)

  let map = parse_map(input)
  let cost = calculate_cost(map)

  io.println(string.from_int(cost))
}
