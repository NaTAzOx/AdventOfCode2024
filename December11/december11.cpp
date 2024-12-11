#include <string>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

vector<string> split(string& str, string& pattern) {
  vector<string> tokens;
  size_t pos = 0;
  string token;
  while ((pos = str.find(pattern)) != string::npos) {
    token = str.substr(0 ,pos);
    tokens.push_back(token);
    str.erase(0, pos + pattern.length());
  }
  tokens.push_back(str);
  
  return tokens;
}

int countDigits(int number) {
  if (number == 0) {
    return 1;
  }
  
  int count = 0;
  
  while (number != 0) {
    number = number / 10;
    count++;
  }
  
  return count;
}

vector<int> process(vector<int> previousNumbers) {
  vector<int> result;
  for (auto number : previousNumbers) {
    int digits = countDigits(number);
    
    if (digits % 2 == 0) {
      string numberToSplit = to_string(number);
      int firstHalf = stoi(numberToSplit.substr(0, numberToSplit.length() / 2));
      int secondHalf = stoi(numberToSplit.substr(numberToSplit.length() / 2));
      result.push_back(firstHalf);
      result.push_back(secondHalf);
    } else {
      if (number == 0) {
        result.push_back(1);
      } else {
        result.push_back(number * 2024);
      }
    }
  }
  return result;
}

int main() {
	ifstream file("input");
	string str;
	getline(file, str);
	vector<int> numbers;
	int number;
	string pattern = " ";
	int occurrence = 1;
	
	for (auto numChar : split(str, pattern)) {
	  number = stoi(numChar);
	  cout << number << endl;
	  numbers.push_back(number);
	}
	
	while (occurrence <= 25) {
	  numbers = process(numbers);
	  occurrence++;
	}
	
	cout << numbers.size() << endl;
}