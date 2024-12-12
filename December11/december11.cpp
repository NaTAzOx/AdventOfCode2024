#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <cstdint>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
using namespace std;

vector<string_view> split(const string& str, const string& pattern) {
    vector<string_view> tokens;
    size_t pos = 0, start = 0;
    while ((pos = str.find(pattern, start)) != string::npos) {
        tokens.emplace_back(str.data() + start, pos - start);
        start = pos + pattern.length();
    }
    tokens.emplace_back(str.data() + start, str.length() - start);
    return tokens;
}

int countDigits(uint64_t number) {
    int digits = 0;
    do {
        ++digits;
        number /= 10;
    } while (number != 0);
    return digits;
}

void process(unordered_map<uint64_t, uint64_t>& numbers) {
    unordered_map<uint64_t, uint64_t> result;
    for (const auto& [number, count] : numbers) {
        int digits = countDigits(number);
        if (digits % 2 == 0) {
            string numberToSplit = to_string(number);
            size_t halfLength = numberToSplit.length() / 2;
            uint64_t firstHalf = stoull(numberToSplit.substr(0, halfLength));
            uint64_t secondHalf = stoull(numberToSplit.substr(halfLength));
            result[firstHalf] += count;
            result[secondHalf] += count;
        } else {
            uint64_t newNumber = (number == 0) ? 1 : number * 2024;
            result[newNumber] += count;
        }
    }
    numbers = move(result);
}

int main() {
    ifstream file("input.txt");
    string str;
    getline(file, str);
    unordered_map<uint64_t, uint64_t> numbers;
    string pattern = " ";
    for (const auto& numChar : split(str, pattern)) {
        if (!numChar.empty()) {
            try {
                uint64_t number = stoull(string(numChar));
                numbers[number]++;
            } catch (const invalid_argument& e) {
                cerr << "Invalid argument: " << numChar << " cannot be converted to uint64_t" << endl;
            } catch (const out_of_range& e) {
                cerr << "Out of range: " << numChar << " is out of range for uint64_t" << endl;
            }
        }
    }

    uint64_t result = 0;

    // part 1
    for (int occurrence = 1; occurrence <= 25; ++occurrence) {
        process(numbers);
        cout << occurrence << endl;
    }

    for (auto [number, count] : numbers) {
        result += count;
    }

    cout << result << endl;

    // part 2
    for (int occurrence = 26; occurrence <= 75; ++occurrence) {
        process(numbers);
        cout << occurrence << endl;
    }

    result = 0;

    for (auto [number, count] : numbers) {
        result += count;
    }

    cout << result << endl;
}