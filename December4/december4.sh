#!/bin/bash

declare -a grid
rows=0

while IFS= read -r line; do
    cols=0
    for char in $line; do
        grid[$((rows * 10 + cols))]=$char
        ((cols++))
    done
    ((rows++))
done <input.txt

check_neighbour() {
    local row=$1
    local col=$2
    local from_char="X"
    local to_char="M"
    local indices=()

    local index=$((row * cols + col))
    if [[ "${grid[$index]}" != "$from_char" ]]; then
        echo "C'est perdu"
        return 1
    fi

    for row_direction in -1 0 1; do
        for column_direction in -1 0 1; do
            if [[ $row_direction -eq 0 && $column_direction -eq 0 ]]; then
                continue
            fi

            local neighbour_on_row=$((row + row_direction))
            local neighbour_on_column=$((col + column_direction))

            if (( neighbour_on_row >= 0 && neighbour_on_row < rows && neighbour_on_column >= 0 && neighbour_on_column < cols )); then
                local neighbour=$((neighbour_on_row * cols + neighbour_on_column))
                if [[ "${grid[$neighbour]}" == "$to_char" ]]; then
                    indices+=("$neighbour")
                fi
            fi
        done
    done

    echo "${indices[@]}"
}

for ((row = 0; row < rows; row++)); do
    for ((col = 0; col < cols; col++)); do
        if [[ "${grid[$row, $col]}" == "X" ]]; then
            neighbours=$(check_neighbour row col)
            echo "$neighbours"
        fi
    done
done