//#include <stdio.h>
//#include <stdlib.h>
//#include <time.h>
//#include <SWI-Prolog.h>
//
///* creates a prolog list by copying the n integer values */
//void create_list_int(term_t lst, int n, int* values) {
//	int i;
//	term_t h = PL_new_term_ref();
//	PL_put_nil(lst); //initialize with the empty list
//	for (i = n - 1; i >= 0; --i) {
//		PL_put_integer(h, values[i]);
//		PL_cons_list(lst, h, lst); //add h in front of the list
//	}
//}
//
///* writes a prolog list of integers */
//void write_list_int(term_t lst) {
//	//the list ref will get modified so copy it
//	term_t tail = PL_copy_term_ref(lst);
//	term_t head = PL_new_term_ref();
//	int first = 1;
//	int x;
//	printf("[");
//	while (PL_get_list(tail, head, tail)) {
//		if (first) {
//			first = 0;
//		}
//		else {
//			printf(", ");
//		}
//		PL_get_integer(head, &x);
//		printf("%d", x);
//	}
//	printf("]");
//}
//
///* writes all the valid decompositions of the given list */
//void decomposeList(int size, int* values) {
//	printf("Will show all decompositions of the given list:\n");
//	//create an array of 3 terms, useful for append
//	term_t t = PL_new_term_refs(3);
//	//first two terms are variables
//	PL_put_variable(t);
//	PL_put_variable(t + 1);
//	//third term is a list of integers
//	create_list_int(t + 2, size, values);
//
//	//get the append predicate from the engine
//	predicate_t p_append = PL_predicate("append", 3, "database");
//	//prepare a query on the "append" predicate
//	qid_t query = PL_open_query(NULL, PL_Q_NORMAL, p_append, t);
//	int result = FALSE;
//	char* L1 = NULL, * L2 = NULL;
//	do {
//		result = PL_next_solution(query);
//		if (result) {
//			printf("L1=");
//			write_list_int(t);
//			printf("; L2=");
//			write_list_int(t + 1);
//			printf("\n");
//		}
//	} while (result);
//	PL_close_query(query);
//}
//
//void consultFile(const char* fileName) {
//	predicate_t p_consult = PL_predicate("consult", 1, "database");
//	term_t t = PL_new_term_ref();
//	PL_put_string_chars(t, fileName);
//	PL_call_predicate(NULL, 0, p_consult, t);
//}
//
///* extract the k-th element of the given list */
//void extractKth(int size, int* values, int k) {
//	printf("Will extract element number %d from the given list:\n", k);
//	//create an array of 4 terms, useful for extract
//	term_t t = PL_new_term_refs(4);
//	//first term is the list
//	create_list_int(t, size, values);
//	//second term is k
//	PL_put_integer(t + 1, k);
//	//last two terms are variables
//	PL_put_variable(t + 2);
//	PL_put_variable(t + 3);
//
//	//get the extract predicate from the engine
//	predicate_t p_extract = PL_predicate("extract", 4, "database");
//	//prepare a query on the "extract" predicate
//	qid_t query = PL_open_query(NULL, PL_Q_NORMAL, p_extract, t);
//	int result = FALSE;
//	do {
//		result = PL_next_solution(query);
//		if (result) {
//			int x;
//			//get the extracted element
//			PL_get_integer(t + 2, &x);
//			printf("Elt=%d; Remaining=", x);
//			write_list_int(t + 3);
//			printf("\n");
//		}
//	} while (result);
//	PL_close_query(query);
//}
//
//foreign_t pl_myrand(term_t t) {
//	int maxVal = 0, r;
//	PL_get_integer(t, &maxVal);
//	r = rand() % maxVal;
//	printf("called: myrand(%d, %d)\n", maxVal, r);
//	return PL_unify_integer(t + 1, r);
//}
//
///* extract k random elements from a list */
//void extractKRandom(int size, int* values, int k) {
//	printf("Will extract %d random numbers from the given list:\n", k);
//	//create an array of 3 terms, useful for extract
//	term_t t = PL_new_term_refs(3);
//	//first term is the list
//	create_list_int(t, size, values);
//	//second term is k
//	PL_put_integer(t + 1, k);
//	//last terms is a variable
//	PL_put_variable(t + 2);
//
//	//get the rnd_select predicate from the engine
//	predicate_t p_rnd_select = PL_predicate("rnd_select", 3, "database");
//	//prepare a query on the "rnd_select" predicate
//	qid_t query = PL_open_query(NULL, PL_Q_NORMAL, p_rnd_select, t);
//	int result = PL_next_solution(query);
//	if (result) {
//		printf("Selected elements = ");
//		write_list_int(t + 2);
//		printf("\n");
//	}
//	PL_close_query(query);
//}
//
//int main(int argc, char** argv) {
//	//initialize Prolog engine
//	//Environment variable SWI_HOME_DIR=c:\Program Files\swipl must be set
//	PL_initialise(argc, argv);
//
//	//initialize random number generator
//	srand((unsigned int)time(NULL));
//	int v[] = { 10, 20, 30, 40, 50, 60, 70, 80 };
//	int size = sizeof(v) / sizeof(v[0]);
//
//	//decompose a list using the built-in "append" predicate
//	decomposeList(size, v);
//	//register pl_myrand as a predicate
//	PL_register_foreign("myrand", 2, pl_myrand, 0);
//	//consult "mylib.pl", where myrand is used
//	consultFile("mylib.pl");
//	//query predicates from mylib
//	extractKth(size, v, 3);
//	extractKRandom(size, v, 4);
//
//	return 0;
//}
