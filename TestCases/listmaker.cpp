#include<iostream>
#include<fstream>
using namespace std;

int main(){
	ifstream infile("line-puffer-superstable.txt");
	ofstream outfile("puffer-train.txt");
    int i = 0;
	while(!infile.eof()){
		string line;
		getline(infile,line);
		for(int j = 0; j < line.length(); j++){
			char c;
			c = line[j];
			if (c == 'o') {outfile << "(2d-vector-set! matrix " << i << " " << j << " (point " << i <<" "<< j << " 1))"<<endl;}
		}
		i++;
		outfile << endl;
	}
}
