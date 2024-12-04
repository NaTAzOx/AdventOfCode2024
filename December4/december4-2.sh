#!/bin/bash

declare -a grid
rows=0
cols=0

# Read the input file and populate the grid
while IFS= read -r line; do
    cols=${#line}  # Set cols to the length of the current line
    for ((i=0; i<cols; i++)); do
        char="${line:$i:1}"
        grid[$((rows * cols + i))]=$char
    done
    ((rows++))
done <input.txt

check_pattern_valid() {
    local row=$1
    local col=$2

    if [[ "$row" -lt 1 || "$row" -ge "$((rows-1))" || "$col" -lt 1 || "$col" -ge "$((cols-1))" ]]; then
        return 1
    fi

    if [[ "${grid[$(((row-1) * cols + (col-1)))]}" == "M" && "${grid[$(((row-1) * cols + (col+1)))]}" == "M" ]]; then
        if [[ "${grid[$(((row+1) * cols + (col-1)))]}" == "S" && "${grid[$(((row+1) * cols + (col+1)))]}" == "S" ]]; then
            return 0
        fi
    fi
    if [[ "${grid[$(((row-1) * cols + (col-1)))]}" == "S" && "${grid[$(((row-1) * cols + (col+1)))]}" == "S" ]]; then
        if [[ "${grid[$(((row+1) * cols + (col-1)))]}" == "M" && "${grid[$(((row+1) * cols + (col+1)))]}" == "M" ]]; then
            return 0
        fi
    fi
    if [[ "${grid[$(((row-1) * cols + (col+1)))]}" == "M" && "${grid[$(((row+1) * cols + (col+1)))]}" == "M" ]]; then
        if [[ "${grid[$(((row-1) * cols + (col-1)))]}" == "S" && "${grid[$(((row+1) * cols + (col-1)))]}" == "S" ]]; then
            return 0
        fi
    fi
    if [[ "${grid[$(((row-1) * cols + (col+1)))]}" == "S" && "${grid[$(((row+1) * cols + (col+1)))]}" == "S" ]]; then
        if [[ "${grid[$(((row-1) * cols + (col-1)))]}" == "M" && "${grid[$(((row+1) * cols + (col-1)))]}" == "M" ]]; then
            return 0
        fi
    fi
    return 1
}

counter=0

# Main loop to check for sequences
for ((row = 0; row < rows; row++)); do
    for ((col = 0; col < cols; col++)); do
        if [[ "${grid[$((row * cols + col))]}" == "A" ]]; then
            if check_pattern_valid $row $col; then
                ((counter++))
            fi
        fi
    done
done

echo "Total sequences found: $counter"