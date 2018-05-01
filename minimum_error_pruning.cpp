/*
	Decision Tree with Minimum Error Pruning
	
	Language : C++
*/

#include<bits/stdc++.h>
using namespace std;

#define False 0
#define True 1
#define MAX 100009
#define Max_String_Length 200
#define Max_Example_Length  500

struct Example{
	char Line[Max_Example_Length];
	short Group_No;
	short Status;
};
int global_count;
float ACCURACY[MAX];

// Structure of a Decision Tree node

typedef struct node node;
struct node{
	string spliton;
	string label;
	string lclass;
	bool isleaf;
	vector<string>children_values;
	vector<node*>children;
	
	// for minimum error pruning
	int total_cnt;
	int gts_cnt;
	int total_class;
	double Ek;
	bool doprune;
};



void parse(string& str,vector<vector<string> >& data_table){
	vector<string>vs;
	while(str.length()!=0&&str.find(',')!=string::npos){
		size_t delimeter_pos;
		string attribute_str;
		delimeter_pos=str.find_first_of(',');
		attribute_str=str.substr(0,delimeter_pos);
		vs.push_back(attribute_str);
		str.erase(0,delimeter_pos+1);
	}
	vs.push_back(str);
	data_table.push_back(vs);
	vs.clear();
}
vector<vector<string> > generate_info(vector<vector<string> >& data_table){
	vector<vector<string> >table_info;
	for(int i=0;i<data_table[0].size();++i){
		vector<string>temp_info;
		map<string,int>temp_map;
		for(int j=0;j<data_table.size();++j){
			if(temp_map.count(data_table[j][i])==0){
				temp_map[data_table[j][i]]=1;
				temp_info.push_back(data_table[j][i]);
			}
			else{
				temp_map[data_table[j][i]]++;
			}
		}
		table_info.push_back(temp_info);
	}
	return table_info;
}
bool table_is_empty(vector<vector<string> >& table){
	return (table.size()==1);
}
bool is_homogeneous(vector<vector<string> >& table){
	int last_col=table[0].size()-1;
	string first_value=table[1][last_col];
	for (int i=1;i<table.size();++i){
		if(first_value!=table[i][last_col]) {
			return false;
		}
	}
	return true;
}
vector<int> count_distinct(vector<vector<string> > &table, int column){
	vector<string> vectorOfStrings;
	vector<int> counts;
	bool found = false;
	int foundIndex;
	for (int iii = 1; iii < table.size(); iii++) {
		for (int jjj = 0; jjj < vectorOfStrings.size(); jjj++) {
			if (vectorOfStrings[jjj] == table[iii][column]) {
				found = true;
				foundIndex = jjj;
				break;
			} else {
				found = false;
			}
		}
		if (!found) {
			counts.push_back(1);
			vectorOfStrings.push_back(table[iii][column]);
		} else {
			counts[foundIndex]++;
		}
	}
	int sum = 0;
	for (int iii = 0; iii < counts.size(); iii++) {
		sum += counts[iii];
	}
	counts.push_back(sum);
	return counts;
}
vector<vector<string> > pruneTable(vector<vector<string> > &attributeTable, string &colName, string value){
	int iii, jjj;
	vector<vector<string> > prunedTable;
	int column = -1;
	vector<string> headerRow;
	for (iii = 0; iii < attributeTable[0].size(); iii++) {
		if (attributeTable[0][iii] == colName) {
			column = iii;
			break;
		}
	}
	for (iii = 0; iii < attributeTable[0].size(); iii++) {
		 if (iii != column) {
		 	headerRow.push_back(attributeTable[0][iii]);
		 }
	}
	prunedTable.push_back(headerRow);
	for (iii = 0; iii < attributeTable.size(); iii++) {
		vector<string> auxRow;
		if (attributeTable[iii][column] == value) {
			for (jjj = 0; jjj < attributeTable[iii].size(); jjj++) {
				if(jjj != column) {
					auxRow.push_back(attributeTable[iii][jjj]);
				}
			}
			prunedTable.push_back(auxRow);
		}
	}
	return prunedTable;
}
string decide_splitting_column(vector<vector<string> >& table){
	int column,i;
	double max_ratio=DBL_MIN;
	int splittingColumn=0;
	vector<int>entropies;
	
	map<string,int>mp; map<string,int>::iterator ii;
	for(int i=1;i<table.size();++i){
		if(mp.find(table[i][table[0].size()-1])!=mp.end()){
			mp[table[i][table[0].size()-1]]++;
		}
		else{
			mp[table[i][table[0].size()-1]]=1;
		}
	}
	double infoT=0.0;
	for(ii=mp.begin();ii!=mp.end();++ii){
		infoT-=((double)ii->second)*(log((double)ii->second/((double)table.size()-1)) / log(2));
	}
	infoT=infoT/((double)table.size()-1);
	
	for(column=0;column<table[0].size()-1;++column){
		string col_name=table[0][column];
		map<string,int>tempMap;
		vector<int>counts=count_distinct(table,column);
		vector<double>attribute_entropy;
		double columnEntropy = 0.0;
		for (i = 1;i < table.size()-1; ++i){
			double entropy = 0.0;
			if (tempMap.find(table[i][column]) != tempMap.end()) { 
				tempMap[table[i][column]]++;
			} 
			else { 	
				tempMap[table[i][column]] = 1;
				vector<vector<string> > tempTable = pruneTable(table, col_name, table[i][column]);
				vector<int> classCounts = count_distinct(tempTable, tempTable[0].size()-1);
				int j, k;
				for (j = 0; j < classCounts.size(); ++j) {
					double temp = (double) classCounts[j];
					entropy -= (temp/classCounts[classCounts.size()-1])*(log(temp/classCounts[classCounts.size()-1]) / log(2));
				}
				attribute_entropy.push_back(entropy);
				entropy = 0.0;
			}
		}
		double split_info=0.0;
		for (i = 0; i < counts.size() - 1; ++i) {
			split_info -= ((double)counts[i]*(log((double)counts[i]/((double) counts[counts.size() - 1]))/log(2)));
		}
		split_info = split_info / ((double) counts[counts.size() - 1]);
		for (i = 0; i < counts.size() - 1; ++i) {
			columnEntropy += ((double) counts[i] * (double) attribute_entropy[i]);
		}
		columnEntropy = columnEntropy / ((double) counts[counts.size() - 1]);
		double gainx=infoT-columnEntropy;
		double gain_ratio=gainx/split_info;
		
		if(gain_ratio>=max_ratio){
			max_ratio=gain_ratio;
			splittingColumn=column;
		}
	}
	
	return table[0][splittingColumn];
}
int return_column_index(string &columnName, vector<vector<string> > &tableInfo){
	int iii;
	for (iii = 0; iii < tableInfo.size(); iii++) {
		if (tableInfo[iii][0] == columnName) {
			return iii;
		}
	}
	return -1;
}
map<string,int> calculate_essentials(vector<vector<string> >& table){
	map<string,int>mp; map<string,int>::iterator ii;
	for(int i=1;i<table.size();++i){
		if(mp.find(table[i][table[0].size()-1])!=mp.end()){
			mp[table[i][table[0].size()-1]]++;
		}
		else{
			mp[table[i][table[0].size()-1]]=1;
		}
	}
	return mp;
}
node* build_decision_tree(vector<vector<string> >& table,node* nodeptr,vector<vector<string> >& table_info){
	if(table_is_empty(table)){
		return NULL;
	}
	if(is_homogeneous(table)){
		nodeptr->isleaf=true;
		nodeptr->lclass=table[1][table[1].size()-1];
		nodeptr->label=nodeptr->lclass;
		nodeptr->Ek=0.0;
		nodeptr->doprune=false;
		return nodeptr;
	}
	else{
		string splitting_col=decide_splitting_column(table);
		
		map<string,int>msi=calculate_essentials(table);
		map<string,int>::iterator ii;
		
		nodeptr->gts_cnt=INT_MIN;
		for(ii=msi.begin();ii!=msi.end();++ii){
			if(ii->second>nodeptr->gts_cnt){
				nodeptr->gts_cnt=ii->second;
				nodeptr->lclass=ii->first;
			}
			//nodeptr->gts_cnt=max(nodeptr->gts_cnt,ii->second);
		}
		
		nodeptr->spliton=splitting_col;
		nodeptr->total_cnt=table.size()-1;
		nodeptr->total_class=msi.size();
		
		nodeptr->Ek=((double)(nodeptr->total_cnt-nodeptr->gts_cnt+nodeptr->total_class-1))/((double)(nodeptr->total_cnt+nodeptr->total_class));
		
		int col_index=return_column_index(splitting_col,table_info);
		double cEk=0.0; int num=0;
		for(int i=1;i<table_info[col_index].size();++i){
			node* newNode=(node*) new node;
			newNode->label=table_info[col_index][i];
			nodeptr->children_values.push_back(table_info[col_index][i]);
			newNode->isleaf=false;
			newNode->spliton=splitting_col;
			vector<vector<string> > auxTable=pruneTable(table,splitting_col,table_info[col_index][i]);
			nodeptr->children.push_back(build_decision_tree(auxTable,newNode,table_info));
		}
		
		for(int i=0;i<nodeptr->children.size();++i){
			if(nodeptr->children[i]==NULL) continue;
			cEk+=(nodeptr->children[i]->Ek*nodeptr->children[i]->total_cnt);
			num+=nodeptr->children[i]->total_cnt;
		}
		
		cEk=(cEk/(double)num);
		if(nodeptr->Ek>=cEk){
			// do not prune
			nodeptr->doprune=false;
		}
		else{
			nodeptr->doprune=true;
		}
	}
	return nodeptr;
}
void printDecisionTree(node* nodePtr,int cnt){
	if(nodePtr == NULL){
		return;
	}
	if(!nodePtr->children.empty()){
		cout << " Parent : "<<cnt<<endl;
		cout << " Value: " << nodePtr->label << endl;
		cout << "Split on: " << nodePtr->spliton;
		int iii;
		for (iii = 0; iii < nodePtr->children.size(); iii++) {   
			cout <<endl<<endl;
			printDecisionTree(nodePtr->children[iii],cnt+1);
		}
		return;
    }
	else{
		cout << " Parent : "<<cnt<<endl;
		cout << " Value: " << nodePtr->label << endl;
		cout << "Predicted class = " << nodePtr->lclass;
		return;
	}
}
void printDecisionTree1(node* nodePtr,int cnt){
	if(nodePtr == NULL){
		return;
	}
	if(!nodePtr->children.empty()){
		cout << " Parent : "<<cnt<<endl;
		cout << " Value: " << nodePtr->label << endl;
		cout << "Split on: " << nodePtr->spliton<<endl;
		
		//cout<<nodePtr->Ek<<" "<<nodePtr->doprune<<endl;
		if(nodePtr->doprune){
			cout << "Predicted class = " << nodePtr->lclass;
			return;
		} 
		int iii;
		for (iii = 0; iii < nodePtr->children.size(); iii++) {   
			cout <<endl<<endl;
			printDecisionTree1(nodePtr->children[iii],cnt+1);
		}
		return;
    }
	else{
		cout << " Parent : "<<cnt<<endl;
		cout << " Value: " << nodePtr->label << endl;
		cout << "Predicted class = " << nodePtr->lclass;
		return;
	}
}
string returnMostFrequentClass(vector<vector<string> > &dataTable){
	map<string,int> trainingClasses;           				
	for (int iii = 1; iii < dataTable.size(); iii++) {
		if (trainingClasses.count(dataTable[iii][dataTable[0].size()-1]) == 0) {
			trainingClasses[dataTable[iii][dataTable[0].size()-1]] = 1;
		} else {
			trainingClasses[dataTable[iii][dataTable[0].size()-1]]++;
		}
	}   
	map<string,int>::iterator mapIter;
	int highestClassCount = 0;
	string mostFrequentClass;
	for (mapIter = trainingClasses.begin(); mapIter != trainingClasses.end(); mapIter++) {
		if (mapIter->second >= highestClassCount) {
			highestClassCount = mapIter->second;
			mostFrequentClass = mapIter->first;
		}   
	}
	return mostFrequentClass;
}
int returnIndexOfVector(vector<string> &stringVector, string value){
	int iii;
	for (iii = 0; iii < stringVector.size(); iii++) {
		if (stringVector[iii] == value)	{
			return iii;
		}
	}
	return -1;
}
string testDataOnDecisionTree(vector<string> &singleLine, node* nodePtr, vector<vector<string> > &tableInfo, string defaultClass){
	string prediction;
	while (!nodePtr->doprune && !nodePtr->isleaf && !nodePtr->children.empty()) {
		int index = return_column_index(nodePtr->spliton, tableInfo);
		string value = singleLine[index];
		int childIndex = returnIndexOfVector(nodePtr->children_values, value);
		nodePtr = nodePtr->children[childIndex];
		if (nodePtr == NULL) {
			prediction = defaultClass;
			break;
		}
		prediction = nodePtr->label;
	}
	return prediction;
}
double printPredictionsAndCalculateAccuracy(vector<string> &givenData, vector<string> &predictions){
	ofstream outputFile;
	outputFile.open("decisionTreeOutput.txt");
	int correct = 0;
	outputFile << setw(3) << "#" << setw(16) << "Given Class" << setw(31) << right << "Predicted Class" << endl;
	outputFile << "--------------------------------------------------" << endl;
	for (int iii = 0; iii < givenData.size(); iii++) {
		outputFile << setw(3) << iii+1 << setw(16) << givenData[iii];
		if (givenData[iii] == predictions[iii]) {
			correct++;
			outputFile << "  ------------  ";
		} else {
			outputFile << "  xxxxxxxxxxxx  ";
		}
		outputFile << predictions[iii] << endl;
	}
	outputFile << "--------------------------------------------------" << endl;
	outputFile << "Total number of instances in test data = " << givenData.size() << endl;
	outputFile << "Number of correctly predicted instances = " << correct << endl;
	outputFile.close();
	float temp = (float) correct/givenData.size() * 100;
	ACCURACY[global_count]=temp;
	global_count++;
}
void C4P5Rules_ten_fold(const char * filename){
	FILE *fp1,*fp2,*k;
    	struct Example *An_Example;
    	int No_Of_Lines,Index,Index1,Limit,i,count=0,count1=0;
   	long int temp;
    	short Increment=1,Group_No=1;
    	char arr[50],filestem[100],example[100],cm[100];
    	float accuracy[10],ava_acc;
    	int acc=0;
    	strcpy(filestem,filename);
   	strcpy(example,filestem);
   	strcat(example,".data");
   	strcpy(cm,filename);
   	strcat(cm,".cm");
   	sprintf(arr,"%s%s%s","wc -l ",example," > temp ");
   	system(arr);
  	fp1=fopen("temp","r");
  	fscanf(fp1,"%d",&No_Of_Lines);
  	fclose(fp1);
  	remove("temp");

  	An_Example=(struct Example *)malloc(sizeof(struct Example)*No_Of_Lines);
   	Index=0;

   	fp1=fopen(example,"r");

   	while(Index<No_Of_Lines){
    		fgets(An_Example[Index].Line,Max_Example_Length,fp1);
     		An_Example[Index].Status=False;
     		Index++;
    	}
   	fclose(fp1);

    	Index=0;
    	Limit=No_Of_Lines/10;
    	Limit*=10;
    	while(Index<Limit){
        	while(1){
			/*temp=rand()%RAND_MAX;*/
	       		temp=rand()%32767;
        		/*temp=(long int)((fa1loat)temp*No_Of_Lines/(RAND_MAX+0.0)); */
            		temp=(long int)((float)temp*No_Of_Lines/(32767+0.0)); 
            		if(temp<No_Of_Lines){
            			if(An_Example[temp].Status==False)
                    			break;
            		}
        	}
        	An_Example[temp].Status=True;
        	An_Example[temp].Group_No=Group_No;
        	Increment++;
        	if(Increment>No_Of_Lines/10){
               		Group_No++;
               		Increment=1;
        	}
        	Index++;
    	}
   	Index=0;
    	Group_No=1;

    	while(Index<No_Of_Lines){
       		if(An_Example[Index].Status==False){
            		An_Example[Index].Group_No=Group_No;
        		Group_No++;
        	}
        	Index++;
    	}

	for(Index=1;Index<=10;Index++){
        	count=Index;count1=Index;
        	sprintf(cm,"%s%s%d%s","_10_fold_",filename,Index,".test");
        	fp1=fopen(cm,"w");
       		sprintf(example,"%s%s%d%s","_10_fold_",filename,Index,".data");
       		fp2=fopen(example,"w");
		fprintf(fp2,"%s",An_Example[0].Line);
		fprintf(fp1,"%s",An_Example[0].Line);
        	for(Index1=1;Index1<No_Of_Lines;Index1++){
            		if(An_Example[Index1].Group_No!=*(&Index)){
               			count++; 
                		fprintf(fp2,"%s",An_Example[Index1].Line);
            		}
        		else{
                		count1++;
                		fprintf(fp1,"%s",An_Example[Index1].Line);
            		}
        	}
        	fclose(fp1);
        	fclose(fp2);
        	//printf("\n%s---->   %d  ,  %s---->   %d",cm,count1-Index,example,count-Index);
    	}

    	//printf("\n");
}
void C4P5Rules_avg_accu_sd(){
	int i;
    	float  AVG_ACCU,SD;
    	float  SUM_ACCU=0,SQR_TOTAL=0;
    	for(i=0;i<global_count;i++){
        	SUM_ACCU=SUM_ACCU+ACCURACY[i];
        	SQR_TOTAL=SQR_TOTAL+ACCURACY[i]*ACCURACY[i];
    	}
 	AVG_ACCU= SUM_ACCU/global_count;
    	SD=sqrt(SQR_TOTAL/global_count - AVG_ACCU*AVG_ACCU);
       
    	printf("\n C4P5Rules's average accuracy= %f\n",AVG_ACCU);
   	printf("\n C4P5Rules's standard deviation= %f\n",SD);
}

int main(int argc,char **argv){
	int ii=1,num_times=0,Track;
	char command[Max_String_Length],command2[Max_String_Length],command1[Max_String_Length];
    	char file_command[Max_String_Length];
	
	if(argc!=9){
    		printf("\\a.out  -m 2 -c 25 -u -f <filestem> <no_of_times>\\ \n");
        	exit(0);
    	}

	num_times=atoi(argv[8]);
	global_count=0;
	while(ii<=num_times){
		cout<<ii<<endl;
		C4P5Rules_ten_fold(argv[7]);
		for(Track=1;Track<=10;Track++){
			sprintf(file_command,"%s%s%d","_10_fold_",argv[7],Track);
        		sprintf(command1,"%s.data",file_command);
        		sprintf(command2,"%s.test",file_command);
           
            		// Training
            
            		ifstream ifs;
			string str;
			vector<vector<string> >data_table;
	
			ifs.open(command1);
	
			if(!ifs){
				cerr << "Error!" << endl;
				exit(-1);
			}
	
			while(getline(ifs,str)){
				parse(str,data_table);
			}
	
			ifs.close(); 
		
			vector<vector<string> >table_info=generate_info(data_table);
	
			node* root=new node;
			root=build_decision_tree(data_table,root,table_info);
	
			string defaultClass = returnMostFrequentClass(data_table);
			data_table.clear(); 
            
	            	// Testing
	            
	            	ifs.clear();
			ifs.open(command2);
			if(!ifs){
				cerr << "Error!" << endl;
				exit(-1);
			}
			while (getline(ifs,str)){ 						
				parse(str,data_table);
			}
			vector<string>predictedClassLabels;										
			vector<string>givenClassLabels;											
			for (int iii = 1; iii < data_table.size(); iii++)				
			{
				string data = data_table[iii][data_table[0].size()-1];
				givenClassLabels.push_back(data);
			}
			for (int iii = 1; iii < data_table.size(); iii++)			
			{
				string someString = testDataOnDecisionTree(data_table[iii], root, table_info, defaultClass);
				predictedClassLabels.push_back(someString);
			}
			data_table.clear();
            
            		/* Print output */
			ofstream outputFile;
			outputFile.open("decisionTreeOutput.txt", ios::app);
			outputFile << endl << "--------------------------------------------------" << endl;
			double accuracy = printPredictionsAndCalculateAccuracy(givenClassLabels, predictedClassLabels);
			outputFile << "Accuracy of decision tree classifier = " << accuracy << "%";
            
			sprintf(command,"rm %s.data",file_command);
			system(command);
			sprintf(command,"rm %s.test",file_command);
			system(command);			
			sprintf(command,"rm decisionTreeOutput.txt");
			system(command);		
		}
		ii++;
	}
	
	C4P5Rules_avg_accu_sd();
	
	return 0;
}
