const fs = require('fs');

// Read input from file
const input = fs.readFileSync('December24/input.txt', 'utf-8').trim().split('\n');

// Parse initial wire values
const wireValues = {};
let i = 0;
while (input[i].includes(':')) {
    const [wire, value] = input[i].split(': ');
    wireValues[wire] = parseInt(value, 10);
    i++;
}

// Parse gate connections
const gates = [];
for (; i < input.length; i++) {
    const [left, operation, right, , output] = input[i].split(' ');
    gates.push({ left, operation, right, output });
}

// Debug: Print parsed input
console.log('Initial wire values:', wireValues);
console.log('Gates:', gates);

// Simulate gates
const simulateGates = (wireValues, gates) => {
    const getValue = (wire) => (wireValues[wire] !== undefined ? wireValues[wire] : null);

    let updated = true;
    while (updated) {
        updated = false;
        gates.forEach(({ left, operation, right, output }) => {
            if (wireValues[output] !== undefined) return; // Skip if output already calculated

            const leftValue = getValue(left);
            const rightValue = getValue(right);

            if (leftValue === null || rightValue === null) return; // Wait for inputs

            let result;
            switch (operation) {
                case 'AND':
                    result = leftValue & rightValue;
                    break;
                case 'OR':
                    result = leftValue | rightValue;
                    break;
                case 'XOR':
                    result = leftValue ^ rightValue;
                    break;
                default:
                    throw new Error(`Unknown operation: ${operation}`);
            }

            wireValues[output] = result;
            updated = true;
        });
    }
};

// Get binary number from wires with a specific prefix
const getBinaryNumberFromWires = (prefix, wireValues) => {
    const binary = Object.keys(wireValues)
        .filter(wire => wire.startsWith(prefix))
        .sort((a, b) => a.localeCompare(b, undefined, { numeric: true }))
        .map(wire => wireValues[wire])
        .reverse() // Reverse to ensure least significant bit first
        .join('');

    console.log(`Binary number for prefix ${prefix}: ${binary}`);
    return binary;
};

// Check if the system performs addition correctly
const findSwapsForAddition = (wireValues, gates) => {
    const maxAttempts = 10000; // Limite maximale pour éviter les boucles infinies
    let attemptCount = 0;

    const xBinary = getBinaryNumberFromWires('x', wireValues);
    const yBinary = getBinaryNumberFromWires('y', wireValues);
    const expectedDecimalSum = parseInt(xBinary, 2) + parseInt(yBinary, 2);

    const allOutputs = gates.map(gate => gate.output);

    const swaps = [];
    for (let i = 0; i < allOutputs.length; i++) {
        for (let j = i + 1; j < allOutputs.length; j++) {
            attemptCount++;
            if (attemptCount > maxAttempts) {
                console.error("Too many attempts, potential infinite loop detected.");
                return "Loop detected";
            }

            const wireValuesCopy = JSON.parse(JSON.stringify(wireValues));
            const gatesCopy = JSON.parse(JSON.stringify(gates));

            // Swap outputs of gates[i] and gates[j]
            const temp = gatesCopy[i].output;
            gatesCopy[i].output = gatesCopy[j].output;
            gatesCopy[j].output = temp;

            simulateGates(wireValuesCopy, gatesCopy);

            const zBinary = getBinaryNumberFromWires('z', wireValuesCopy);
            const actualDecimalSum = parseInt(zBinary, 2);

            if (expectedDecimalSum === actualDecimalSum) {
                swaps.push(gates[i].output, gates[j].output);
            }
        }
    }

    return [...new Set(swaps)].sort((a, b) => a.localeCompare(b)).join(',');
};


// Find the swaps needed
const swapsNeeded = findSwapsForAddition(wireValues, gates);

console.log(`Swaps needed: ${swapsNeeded}`);
