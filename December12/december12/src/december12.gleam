import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/string_tree

type Position {
  Pos(Int, Int)
}

type Map {
  Map(List(#(Position, String)))
}

fn parse_map(input: String) -> Map {
  input
  |> string.split("\n")
  |> list.index_map(fn(_row_index, row) {
    let graphemes = string.to_graphemes(row)
    graphemes
    |> list.index_map(fn(col_index, c) {
      #(Pos(row_index, col_index), string.from_grapheme(c))
    })
  })
  |> list.flatten
  |> Map
}

const directions = [Pos(0, 1), Pos(1, 0), Pos(0, -1), Pos(-1, 0)]

fn within_bounds(map: Map, pos: Position) -> Bool {
  let Map(entries) = map
  list.any(entries, fn(entry) {
    let #(Pos(x, y), _) = entry
    pos == Pos(x, y)
  })
}

fn explore(
  map: Map,
  stack: List(position),
  area: Int,
  perimeter: Int,
  new_visited: List(position),
  char: String,
) -> #(Int, Int, List(Position)) {
  case stack {
    [] -> #(area, perimeter, new_visited)
    [current, ..rest] -> {
      let stack = rest
      let Map(entries) = map

      case
        list.find(entries, fn(entry) {
          let #(_, c) = entry
          current == current && c == char
        })
      {
        Ok(_) -> {
          let new_visited = list.append(new_visited, [current])
          let updated_stack_and_perimeter =
            list.fold(directions, #(stack, perimeter), fn(direction, _) {
              let Pos(dx, dy) = direction
              let Pos(current_x, current_y) = current
              let neighbor = Pos(current_x + dx, current_y + dy)

              case within_bounds(map, neighbor) {
                True ->
                  case list.contains(new_visited, neighbor) {
                    True -> #(stack, perimeter)
                    False -> #(list.append(stack, [neighbor]), perimeter)
                  }
                False -> #(stack, perimeter + 1)
              }
            })
          let #(stack, perimeter) = updated_stack_and_perimeter
          explore(map, stack, area + 1, perimeter, new_visited, char)
        }
        Error(_) -> #(area, perimeter, new_visited)
      }
    }
  }
}

fn calculate_cost(map: Map) -> Int {
  let Map(entries) = map
  let visited = []
  let total_cost = 0

  list.fold(entries, #(visited, total_cost), fn(entry, acc) {
    let #(visited, total_cost) = acc
    let #(pos, char) = entry

    case list.contains(visited, [pos]) {
      True -> #(visited, total_cost)
      False -> {
        let #(_, _, new_visited) =
          explore(map, [pos], 0, 0, visited, string.from_grapheme(char))
        #(list.append(visited, new_visited), total_cost + { area * perimeter })
      }
    }
  }).1
}

// Function to read the contents of a file
fn read_file(filename: String) -> String {
  let result = io.read_file(filename)
  case result {
    Ok(bytes) -> string_tree.to_string(bytes)
    Error(_) -> ""
  }
}

pub fn main() {
  let input = read_file("input.txt")
  let map = parse_map(input)
  let cost = calculate_cost(map)
  io.println(int.to_string(cost))
}
