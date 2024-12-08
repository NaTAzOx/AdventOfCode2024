import * as fs from 'fs';

class Position {
    x: number;
    y: number;

    constructor(x: number, y: number) {
        this.x = x;
        this.y = y;
    }
}

let input = fs.readFileSync('input.txt').toString().split('\n');

let positionsForFrequencies: { [key: string]: Position[] } = {};
let antinodePositions: Set<string> = new Set();
let antinodeCount;
const reproductionCount = 50;

for (let row = 0; row < input.length; row++) {
    let parts = input[row].split('');
    for (let column = 0; column < parts.length; column++) {
        let char = parts[column];
        const regex = /([a-zA-Z0-9])/g;
        if (regex.test(char)) {
            if (positionsForFrequencies[char]) {
                for (const element of positionsForFrequencies[char]) {
                    const deltaX = row - element.x;
                    const deltaY = column - element.y;
                    for (let i = 1; i <= reproductionCount; i++) {
                        const antinodeX = element.x + i * deltaX;
                        const antinodeY = element.y + i * deltaY;
                        const otherSideAntinodeX = element.x - i * deltaX;
                        const otherSideAntinodeY = element.y - i * deltaY;
                        if (
                            antinodeX >= 0 &&
                            antinodeY >= 0 &&
                            antinodeX < input.length &&
                            antinodeY < parts.length
                        ) {
                            antinodePositions.add(`${antinodeX},${antinodeY}`);
                        }

                        if (
                            otherSideAntinodeX >= 0 &&
                            otherSideAntinodeY >= 0 &&
                            otherSideAntinodeX < input.length &&
                            otherSideAntinodeY < parts.length
                        ) {
                            antinodePositions.add(`${otherSideAntinodeX},${otherSideAntinodeY}`);
                        }
                    }
                }
                positionsForFrequencies[char].push(new Position(row, column));
            } else {
                positionsForFrequencies[char] = [new Position(row, column)];
            }
        }
    }
}

for (let char in positionsForFrequencies) {
    const positions = positionsForFrequencies[char];
    if (positions.length > 1) {
        for (const position of positions) {
            antinodePositions.add(`${position.x},${position.y}`);
        }
    }
}

antinodeCount = antinodePositions.size;

console.log(`Antinode Count: ${antinodeCount}`);
