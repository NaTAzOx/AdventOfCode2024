#!/bin/bash

declare -a grid
rows=0
cols=0

while IFS= read -r line; do
    cols=${#line}
    for ((i=0; i<cols; i++)); do
        char="${line:$i:1}"
        grid[$((rows * cols + i))]=$char
    done
    ((rows++))
done <input.txt

echo "Rows: $rows"
echo "Cols: $cols"
echo "Grid: ${grid[@]}"

check_neighbour() {
    local row=$1
    local col=$2
    local from_char="X"
    local to_char="M"
    local indices=()

    local index=$((row * cols + col))
    if [[ "${grid[$index]}" != "$from_char" ]]; then
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
                    indices+=("$neighbour:$neighbour_on_row:$neighbour_on_column:$row_direction:$column_direction")
                fi
            fi
        done
    done

    echo "${indices[@]}"
}

check_sequence() {
    local row=$1
    local col=$2
    local row_direction=$3
    local col_direction=$4

    local next_row=$((row + row_direction))
    local next_col=$((col + col_direction))
    echo "Checking sequence from ($row, $col) in direction ($row_direction, $col_direction)"
    if (( next_row >= 0 && next_row < rows && next_col >= 0 && next_col < cols )); then
        local next_index=$((next_row * cols + next_col))
        echo "Next position: ($next_row, $next_col) with index $next_index and character ${grid[$next_index]}"
        if [[ "${grid[$next_index]}" == "A" ]]; then
            next_row=$((next_row + row_direction))
            next_col=$((next_col + col_direction))
            if (( next_row >= 0 && next_row < rows && next_col >= 0 && next_col < cols )); then
                next_index=$((next_row * cols + next_col))
                echo "Next position: ($next_row, $next_col) with index $next_index and character ${grid[$next_index]}"
                if [[ "${grid[$next_index]}" == "S" ]]; then
                    echo "Found a sequence"
                    return 0
                fi
            fi
        fi
    fi
    return 1
}

counter=0

for ((row = 0; row < rows; row++)); do
    for ((col = 0; col < cols; col++)); do
        if [[ "${grid[$((row * cols + col))]}" == "X" ]]; then
            neighbours=$(check_neighbour $row $col)
            for neighbour in $neighbours; do
                IFS=':' read -r neighbour_index neighbour_row neighbour_column row_direction col_direction <<< "$neighbour"
                if check_sequence $neighbour_row $neighbour_column $row_direction $col_direction; then
                    echo "Heyyyyy"
                    ((counter++))
                fi
            done
        fi
    done
done

echo "Total sequences found: $counter"