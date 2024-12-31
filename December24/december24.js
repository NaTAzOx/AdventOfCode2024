const fs = require('fs');

const input = fs.readFileSync('December24/input.txt', 'utf-8').trim().split('\n');

const wireValues = {};
let i = 0;
while (input[i].includes(':')) {
    const [wire, value] = input[i].split(': ');
    wireValues[wire] = parseInt(value, 10);
    i++;
}

const gates = [];
for (; i < input.length; i++) {
    const [left, operation, right, , output] = input[i].split(' ');
    gates.push({ left, operation, right, output });
}

const simulateGates = (wireValues, gates) => {
    const getValue = (wire) => (wireValues[wire] !== undefined ? wireValues[wire] : null);

    let updated = true;
    while (updated) {
        updated = false;
        gates.forEach(({ left, operation, right, output }) => {
            if (wireValues[output] !== undefined) return;

            const leftValue = getValue(left);
            const rightValue = getValue(right);

            if (leftValue === null || rightValue === null) return;

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

simulateGates(wireValues, gates);

let binaryOutput = '';
Object.keys(wireValues)
    .filter(wire => wire.startsWith('z'))
    .sort((a, b) => a.localeCompare(b, undefined, { numeric: true }))
    .forEach(wire => {
        binaryOutput = wireValues[wire] + binaryOutput;
    });

const decimalOutput = parseInt(binaryOutput, 2);

console.log(`Decimal output: ${decimalOutput}`);
