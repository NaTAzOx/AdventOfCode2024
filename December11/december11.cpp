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
	
	cout << numbers.size() << endl;
}