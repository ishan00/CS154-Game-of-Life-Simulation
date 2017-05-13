#include<iostream>
#include<fstream>
#include<vector>
using namespace std;

bool isNum(char c){
    if ((c > 47) && (c <58)) return true;
    else return false;
}

int charToInt(string num){
    int last = num.length()-1;
    if (num == "") return 0;
    else return ((num[last]-48) + 10*charToInt(num.substr(0,last)));
}

string expand(string line){
    string output = "";
    while(line != ""){
        char c = line[0];
        //cout << c << endl;
        if(isNum(c)){
            string duplicate_line = line.substr(1);
            //cout << "This is duplicate line : "<< duplicate_line << endl;
            int j = 0;
            char d = duplicate_line[0];
            //cout << "This is char d : " << d << endl;
            while(isNum(d)){
                j++;
                duplicate_line = duplicate_line.substr(1);
                //cout << "Duplicate Line within the while loop : " << duplicate_line << endl;
                d = duplicate_line[0];
                //cout << "This is character d within the while loop : " << d << endl;
                }
            int repetition = charToInt(line.substr(0,j+1));
            //cout << "Number of times things were repeated : " << repetition << endl;
            d = line[j+1];
            //cout << "This is d : " << d << endl;
            line = line.substr(j+2);
            //cout << "This is remaining of line to be yet processed : " << line << endl;
            output = output + string(repetition,d);
            }
        else {
            output = output + c;
            line = line.substr(1);
        }
    }
    int length = output.length();
    return output + string(240-length,'b');
}
int main(){
	ifstream infile("vacuum-cleaner.rle");
	vector<string> parsed(0);
	ofstream outfile("vacuum-cleaner.txt");
	string s;
    while(!infile.eof()){
        getline(infile,s);
        parsed.push_back(expand(s));
	}
	for(int i = 0; i<parsed.size();i++){

        outfile <<"(hash-set! matrix "<< i << " (list ";
		for(int j = 0; j < parsed[i].length(); j++){
			char c;
			c = parsed[i][j];
			if (c == 'o') {outfile << j << " ";}
		}
		outfile << "))" << endl;
	}
    }
