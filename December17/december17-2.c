#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MAX_PROGRAM_LENGTH 1000

int get_combo_operand_value(int operand, int A, int B, int C) {
    switch (operand) {
        case 0: return 0;
        case 1: return 1;
        case 2: return 2;
        case 3: return 3;
        case 4: return A;
        case 5: return B;
        case 6: return C;
        default: 
            printf("Invalid combo operand: %d\n", operand);
            exit(1);
    }
}

char* execute_program(int *program, int program_length, int A, int B, int C) {
    static char output_str[4096];
    int output[MAX_PROGRAM_LENGTH];
    int output_count = 0;
    int ip = 0;

    while (ip < program_length) {
        int opcode = program[ip++];
        int operand = program[ip++];

        switch (opcode) {
            case 0: // adv
                A = A / (1 << get_combo_operand_value(operand, A, B, C));
                break;
            case 1: // bxl
                B ^= operand;
                break;
            case 2: // bst
                B = get_combo_operand_value(operand, A, B, C) % 8;
                break;
            case 3: // jnz
                if (A != 0) {
                    ip = operand;
                }
                break;
            case 4: // bxc
                B ^= C;
                break;
            case 5: // out
                output[output_count++] = get_combo_operand_value(operand, A, B, C) % 8;
                break;
            case 6: // bdv
                B = A / (1 << get_combo_operand_value(operand, A, B, C));
                break;
            case 7: // cdv
                C = A / (1 << get_combo_operand_value(operand, A, B, C));
                break;
            default:
                printf("Invalid opcode: %d\n", opcode);
                return NULL;
        }
    }

    char *ptr = output_str;
    for (int i = 0; i < output_count; i++) {
        if (i > 0) {
            *ptr++ = ',';
        }
        ptr += sprintf(ptr, "%d", output[i]);
    }
    *ptr = '\0';

    return output_str;
}

int find_correct_initial_A(int *program, int program_length, int B, int C, const char *program_str) {
    int low = 1, high = 1 << 20; // Reasonable bounds for binary search
    while (low <= high) {
        int mid = low + (high - low) / 2;
        char *output = execute_program(program, program_length, mid, B, C);
        if (output == NULL) {
            return -1; // Error in execution
        }

        int cmp = strcmp(output, program_str);
        if (cmp == 0) {
            return mid; // Found
        } else if (cmp < 0) {
            low = mid + 1; // Go to higher values
        } else {
            high = mid - 1; // Go to lower values
        }
    }

    return -1; // No result found
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Failed to open input.txt");
        return 1;
    }

    int A, B, C;
    fscanf(file, "Register A: %d\n", &A);
    fscanf(file, "Register B: %d\n", &B);
    fscanf(file, "Register C: %d\n", &C);

    int program[MAX_PROGRAM_LENGTH];
    int program_length = 0;

    char line[4096];
    fscanf(file, "Program: ");
    fgets(line, sizeof(line), file);

    char *token = strtok(line, ",");
    while (token != NULL && program_length < MAX_PROGRAM_LENGTH) {
        program[program_length++] = atoi(token);
        token = strtok(NULL, ",");
    }

    fclose(file);

    char program_str[4096];
    char *ptr = program_str;
    for (int i = 0; i < program_length; i++) {
        if (i > 0) {
            *ptr++ = ',';
        }
        ptr += sprintf(ptr, "%d", program[i]);
    }
    *ptr = '\0';

    int result = find_correct_initial_A(program, program_length, B, C, program_str);
    if (result != -1) {
        printf("Lowest positive initial value for register A: %d\n", result);
    } else {
        printf("No valid initial value for register A found.\n");
    }

    return 0;
}
