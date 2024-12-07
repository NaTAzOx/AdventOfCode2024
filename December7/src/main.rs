fn read_file() -> String {
    let contents = std::fs::read_to_string("src/input.txt")
        .expect("Something went wrong reading the file");

    contents
}

fn generate_pattern(operators_needed: usize) -> Vec<Vec<String>> {
    let operators = ["+", "*", "||"];
    let mut patterns = Vec::new();

    let total_combinations = operators.len().pow(operators_needed as u32);

    for i in 0..total_combinations {
        let mut pattern = Vec::new();
        let mut index = i;
        for _ in 0..operators_needed {
            pattern.push(operators[index % operators.len()].to_string());
            index /= operators.len();
        }
        patterns.push(pattern);
    }

    patterns
}

fn main() {
    let lines = read_file();

    let mut result = 0;
    let mut result2 = 0;

    for line in lines.lines() {
        let split_between_result_and_calculation: Vec<&str> = line.split(":").collect();

        let line_result = split_between_result_and_calculation[0].parse::<i128>().unwrap();
        let operands: Vec<&str> = split_between_result_and_calculation[1].split_whitespace().collect();

        let operators_vec = generate_pattern(operands.len() - 1);
        let operators_clone = operators_vec.clone();

        // Part 1
        for operators in operators_vec {
            let mut operands_clone = operands.clone();
            let mut calculation_result = operands_clone.remove(0).parse::<i128>().unwrap();

            for (_, operator) in operators.iter().enumerate() {
                let operand = operands_clone.remove(0).parse::<i128>().unwrap();

                if operator == "+" {
                    calculation_result += operand;
                } else if operator == "*" {
                    calculation_result *= operand;
                }
            }

            if calculation_result == line_result {
                result += calculation_result;
                break;
            }
        }

        // Part 2
        for operators in operators_clone {
            let mut operands_clone = operands.clone();
            let mut calculation_result = operands_clone.remove(0).parse::<i128>().unwrap();

            for (_, operator) in operators.iter().enumerate() {
                let operand = operands_clone.remove(0).parse::<i128>().unwrap();

                if operator == "+" {
                    calculation_result += operand;
                } else if operator == "*" {
                    calculation_result *= operand;
                } else if operator == "||" {
                    calculation_result = format!("{}{}", calculation_result, operand).parse::<i128>().unwrap();
                }
            }

            if calculation_result == line_result {
                result2 += calculation_result;
                break;
            }
        }
    }

    println!("Result for part 1: {}", result);
    println!("Result for part 2: {}", result2);
}