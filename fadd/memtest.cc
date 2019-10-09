
#include <string>
#include <string.h>
#include <iostream>
#include "fadd.h"

long int g_bigramFaddNumber = -1;

using namespace std;


void destroyWordList(list_of_words *list)
	// remove such a linked list from memory
{
	list_of_words 
		*current = list,
		*next;

	while(current != NULL){
		next = current->next;
		delete(current->word);
		delete(current);
		current = next;
	}
}




void destroyNumberList(list_of_numbers *list)
	// remove such a linked list from memory
{
	list_of_numbers 
		*current = list,
		*next;

	while(current != NULL){
		next = current->next;
		delete(current);
		current = next;
	}
}




int loadProbsBigram(const char *tupleFile, const char *dictFile0, const char *dictFile1)
	// initialize bigram data
{
	list_of_words 
		*tuple = new list_of_words,
		*dict0 = new list_of_words,
		*dict1 = new list_of_words;

	tuple->word = tupleFile;
	dict0->word = dictFile0;
	dict1->word = dictFile1;

	tuple->next = dict0;
	dict0->next = dict1;
	dict1->next = NULL;

	g_bigramFaddNumber = init_tuple(tuple);
	
//	destroyWordList(tuple);

	return(g_bigramFaddNumber != -1);
}




int main(int argc, char **argv){
	
	// initialize fadd library
	fadd_init_lib(100);

	string
		dataDir     = "./",
		bigramTuple = dataDir+"bigrams.tpl",
		wordDict    = dataDir+"words.fsa",
		tagDict     = dataDir+"tags.fsa";

	// load fadd data
//	loadProbsBigram(bigramTuple.c_str(),wordDict.c_str(),tagDict.c_str());
	loadProbsBigram("./bigrams.tpl","./tags.fsa","./tags.fsa");



		// create tag bigram
		string
			tag1 = "unseen_tag_1",
			tag2 = "unseen_tag_2";

		// create word list of bigram
		list_of_words
			*wordList = new list_of_words;
		wordList->word = strdup(tag1.c_str());
		wordList->next = new list_of_words;
		wordList->next->word = strdup(tag2.c_str());
		wordList->next->next = NULL;
while(1) {
		// query fadd data for bigram
		list_of_numbers 
			*bigramProbability = word_tuple_grams(wordList,g_bigramFaddNumber);

		// release memory of word list and number list
	//	destroyWordList(wordList);
		destroyNumberList(bigramProbability);
	}

	
	
	// close fadd files and lib
	close_tuple(g_bigramFaddNumber);
	fadd_close_lib();

	return(0);
}
