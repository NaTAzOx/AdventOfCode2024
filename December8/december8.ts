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
let antinodePositions: Position[] = [];
let antinodeCount = 0;

for (let row = 0; row < input.length; row++) {
    let parts = input[row].split('');
    for (let column = 0; column < parts.length; column++) {
        let char = parts[column];
        const regex = /([a-zA-Z0-9])/g;
        if (regex.test(char)) {
            if (positionsForFrequencies[char]) {
                for (const element of positionsForFrequencies[char]) {
                    console.log(element);
                    console.log(`Row: ${row}, Column: ${column}`);
                    const antinodeX = (row - element.x) + row;
                    const antinodeY = (column - element.y) + column;
                    const otherSideAntinodeX = (element.x - row) + element.x;
                    const otherSideAntinodeY = (element.y - column) + element.y;
                    console.log(`Antinode: ${antinodeX}, ${antinodeY}`, `Other Side Antinode: ${otherSideAntinodeX}, ${otherSideAntinodeY}`);
                    if (
                        !antinodePositions.find(p => p.x === antinodeX && p.y === antinodeY) &&
                        antinodeY >= 0 && antinodeX >= 0 &&
                        antinodeX < input.length && antinodeY < parts.length
                    ) {
                        antinodePositions.push(new Position(antinodeX, antinodeY));
                        antinodeCount++;
                    }
                    if (!antinodePositions.find(p => p.x === otherSideAntinodeX && p.y === otherSideAntinodeY) &&
                        otherSideAntinodeY >= 0 && otherSideAntinodeX >= 0 &&
                        otherSideAntinodeX < input.length && otherSideAntinodeY < parts.length
                    ) {
                        antinodePositions.push(new Position(otherSideAntinodeX, otherSideAntinodeY));
                        antinodeCount++;
                    }
                }
                positionsForFrequencies[char].push(new Position(row, column));
            } else {
                positionsForFrequencies[char] = [new Position(row, column)];
            }
        }
    }
}

console.log(`Antinode Count: ${antinodeCount}`);