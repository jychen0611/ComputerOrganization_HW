#include<iostream>
#include<fstream>
#include<stdlib.h>
#include<cmath>
#include<climits>
using namespace std;

class Data{
	public:
		bool valid=false;
		int time=0;
		unsigned int tag=0;
};


int main(int argc,char *argv[]){
	int cache_size,block_size,associativity,replace;
	int blockNum, index, offset, tag_bit, index_bit;
	int n=0,record_time, record_index;
	bool victim;
	unsigned int data,tag;
	
	ifstream fileIn(argv[1], ios::in);
	ofstream fileOut(argv[2], ios::out);
	fileIn >> cache_size >> block_size >> associativity >> replace;
	blockNum = cache_size*1024/block_size;
	offset = log(block_size)/log(2);
	Data *cache = new Data[blockNum];	
	fileIn >> hex;

	if(associativity == 0){//direct-mapped
		index_bit = log(blockNum )/log(2);
		tag_bit = 32-index_bit-offset;
		while(fileIn>>data){
			index = (data<<tag_bit)>>(tag_bit+offset);
			tag=data>>(index_bit+offset);
			if(cache[index].valid==0){
				cache[index].tag=tag;
				fileOut<<-1<<endl;
				cache[index].valid=1;
			}else{
				if(tag==cache[index].tag){
					fileOut<<-1<<endl;
				}else{
					fileOut<<cache[index].tag<<endl;
					cache[index].tag=tag;
				}
			}
		}
	}else if(associativity == 1){//four-way set associative
		index_bit = log(blockNum /4)/log(2);
                tag_bit = 32 - index_bit - offset;
                while(fileIn >> data) {
                        ++n;
                        index = (data << tag_bit) >> (tag_bit + offset);
                        tag = data >> (index_bit + offset);
			//if(replace==2)
			//	record_time = 0;
			//else
			if(replace!=2)
				record_time = INT_MAX;
                        victim = true;
                        //search data   
                        for(int i = 0; i < 4; ++i) {
                                if(cache[index*4+i].valid == 1 && tag == cache[index*4+i].tag) {
                                        if(replace == 1)//LRU
                                                cache[index*4+i].time = n;
                                        fileOut << -1 << endl;
                                        victim = false;
                                        break;
                                }
                        }
                        if(victim) {
                                for(int i = 0; i < 4; ++i) {
                                        if(cache[index*4+i].valid == 0) {
                                                cache[index*4+i].valid = 1;
                                                cache[index*4+i].tag = tag;
                                                cache[index*4+i].time = n;
                                                fileOut << -1 << endl;
                                                victim = false;
                                                break;
                                        }
                                }
                        }
                        //determine victim
			if(victim) {
                                if(replace == 2) {//myself
                                                        record_index = index*4+0;
                                }
                                else {//LRU
                                        for(int i = 0; i < 4; ++i) {
                                                if(record_time > cache[index*4+i].time) {
                                                        record_time = cache[index*4+i].time;
                                                        record_index = index*4+i;
                                                }
                                        }
                                }
                                fileOut << cache[record_index].tag << endl;
                                cache[record_index].time = n;
                                cache[record_index].tag = tag;
                        }
		}
	}else if(associativity == 2){//fully associative
		tag_bit = 32 - offset;
                while(fileIn >> data) {
                        ++n;
                        tag = data >> offset;
			if(replace!=2)
				record_time = INT_MAX;
                        victim = true;
                        //search data
                        for(int i = 0; i < blockNum ; ++i) {
                                if(cache[i].valid == 1 && tag == cache[i].tag) {
                                        if(replace == 1)//LRU
                                                cache[i].time = n;
                                        fileOut << -1 << endl;
                                        victim = false;
                                        break;
                                }
                        }
                        //search vacancy
			if(victim) {
                                for(int i = 0; i < blockNum ; ++i) {
                                        if(cache[i].valid == 0) {
                                                cache[i].valid = 1;
                                                cache[i].tag = tag;
                                                cache[i].time = n;
                                                fileOut << -1 << endl;
                                                victim = false;
                                                break;
                                        }
                                }
                        }
                        //determine victim
			if(victim) {
                                if(replace == 2) {//myself
                                                        record_index = 0;
                                }
                                else {//LRU
                                        for(int i = 0; i < blockNum ; ++i) {
                                                if(record_time > cache[i].time) {
                                                        record_time = cache[i].time;
                                                        record_index = i;
                                                }
                                        }
                                }
                                fileOut << cache[record_index].tag << endl;
                                cache[record_index].time = n;
                                cache[record_index].tag = tag;
                        }
		}
	}

	return 0;
}
