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
                A = A / (int)pow(2, get_combo_operand_value(operand, A, B, C));
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
                B = A / (int)pow(2, get_combo_operand_value(operand, A, B, C));
                break;
            case 7: // cdv
                C = A / (int)pow(2, get_combo_operand_value(operand, A, B, C));
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

    char *output = execute_program(program, program_length, A, B, C);
    if (output) {
        printf("%s\n", output);
    }

    return 0;
}