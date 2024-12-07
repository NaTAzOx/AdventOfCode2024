package main

import (
	"fmt"
	"os"
	"strings"
)

func advance(posX, posY int, char string) (int, int) {
	switch char {
	case "^":
		posY--
	case "v":
		posY++
	case ">":
		posX++
	case "<":
		posX--
	}
	return posX, posY
}

func right(char string) string {
	switch char {
	case "^":
		return ">"
	case ">":
		return "v"
	case "v":
		return "<"
	case "<":
		return "^"
	}
	return ""
}

func reachedAnObstruction(char string, posX, posY int, charArrayArrays [][]string) bool {
	if char == "^" && (posY-1 < 0 || charArrayArrays[posY-1][posX] == "#") {
		return true
	} else if char == "v" && (posY+1 >= len(charArrayArrays) || charArrayArrays[posY+1][posX] == "#") {
		return true
	} else if char == ">" && (posX+1 >= len(charArrayArrays[0]) || charArrayArrays[posY][posX+1] == "#") {
		return true
	} else if char == "<" && (posX-1 < 0 || charArrayArrays[posY][posX-1] == "#") {
		return true
	}
	return false
}

func changeInArray(charArrayArrays [][]string, oldPosX, oldPosY, posX, posY int, char string) {
	if oldPosX < 0 || oldPosX >= len(charArrayArrays[0]) || oldPosY < 0 || oldPosY >= len(charArrayArrays) {
		return
	}
	if posX < 0 || posX >= len(charArrayArrays[0]) || posY < 0 || posY >= len(charArrayArrays) {
		return
	}
	charArrayArrays[oldPosY][oldPosX] = "X"
	charArrayArrays[posY][posX] = char
}

func copyGrid(grid [][]string) [][]string {
	newGrid := make([][]string, len(grid))
	for i := range grid {
		newGrid[i] = make([]string, len(grid[i]))
		copy(newGrid[i], grid[i])
	}
	return newGrid
}

func isLooped(posX, posY int, charArrayArrays [][]string, row, col int) bool {
	gridCopy := copyGrid(charArrayArrays)
	steppedIn := make(map[[2]int]string)
	for {
		if posX < 0 || posX >= col || posY < 0 || posY >= row {
			break
		}
		var char = gridCopy[posY][posX]
		var oldPosX, oldPosY = posX, posY
		if posX-1 < 0 || posX+1 >= col || posY-1 < 0 || posY+1 >= row {
			posX, posY = advance(posX, posY, char)
		} else if reachedAnObstruction(char, posX, posY, gridCopy) {
			char = right(char)
		} else {
			posX, posY = advance(posX, posY, char)
			if steppedIn[[2]int{posX, posY}] == char {
				return true
			} else {
				steppedIn[[2]int{posX, posY}] = char
			}
		}
		changeInArray(gridCopy, oldPosX, oldPosY, posX, posY, char)
	}
	return false
}

func main() {
	data, err := os.ReadFile("December6/input.txt")
	if err != nil {
		fmt.Println("File reading error", err)
		return
	}

	lines := strings.Split(string(data), "\n")
	var charArrayArrays [][]string
	var posX, posY int
	var row, col = 0, 0
	var count = 0

	for _, line := range lines {
		var charArray []string
		col = 0
		for _, char := range line {
			if char != '\n' && char != '#' && char != '.' {
				posX = col
				posY = row
			}
			charArray = append(charArray, string(char))
			col++
		}
		charArrayArrays = append(charArrayArrays, charArray)
		row++
	}

	col = len(charArrayArrays[0])
	spawnX, spawnY := posX, posY

	for i := 0; i < len(charArrayArrays); i++ {
		for j := 0; j < len(charArrayArrays[i]); j++ {
			if i == spawnY && j == spawnX || charArrayArrays[i][j] == "#" {
				continue
			}
			charArrayArrays[i][j] = "#"
			if isLooped(posX, posY, charArrayArrays, row, col) {
				count++
			}
			charArrayArrays[i][j] = "."
		}
	}

	fmt.Println(count)
}
