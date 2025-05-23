//#include <stdio.h>
//#include <SWI-Prolog.h>
//
//#define GRID_SIZE 9
//
//void consultFile(const char *fileName){
//	predicate_t p_consult = PL_predicate("consult", 1, "database");
//	term_t t = PL_new_term_ref();
//	PL_put_string_chars(t, fileName);
//	PL_call_predicate(NULL, 0, p_consult, t);
//}
//
//void create_grid(term_t lst, char rows[][GRID_SIZE + 2]) {
//	PL_put_nil(lst);
//	for (int i = GRID_SIZE - 1; i >= 0; --i) {
//		term_t row = PL_new_term_ref();
//		PL_put_nil(row);
//		for (int j = GRID_SIZE - 1; j >= 0; --j) {
//			if (rows[i][j] == '_') {
//				term_t h = PL_new_term_ref();
//				PL_put_variable(h);
//				PL_cons_list(row, h, row);
//			}
//			else {
//				term_t h = PL_new_term_ref();
//				PL_put_integer(h, rows[i][j] - '0');
//				PL_cons_list(row, h, row);
//			}
//		}
//		PL_cons_list(lst, row, lst);
//	}
//}
//
//void show_grid(term_t lst) {
//	term_t tail = PL_copy_term_ref(lst);
//	term_t head = PL_new_term_ref();
//	int x;
//	while (PL_get_list(tail, head, tail)) {
//		term_t head2 = PL_new_term_ref();
//		term_t tail2 = PL_copy_term_ref(head);
//		while (PL_get_list(tail2, head2, tail2)) {
//			PL_get_integer(head2, &x);
//			printf("%d ", x);
//		}
//		printf("\n");
//	}
//}
//
//void solve(const char *fileName){
//	//read the sudoku grid
//	FILE *f;
//	fopen_s(&f, fileName, "r");
//	char rows[GRID_SIZE][GRID_SIZE+2];
//	int i;
//	for(i=0; i<GRID_SIZE; ++i){
//		fgets(rows[i], 11, f);
//	}
//	
//	//prepare the sudoku predicate
//	predicate_t p_sudoku = PL_predicate("sudoku", 1, "database");
//	term_t t = PL_new_term_ref();
//	create_grid(t, rows);
//	qid_t query = PL_open_query(NULL, PL_Q_NORMAL, p_sudoku, t);
//	int result = PL_next_solution(query);
//	if(result){
//		printf("Solution found:\n");
//		show_grid(t);
//		printf("\n");
//	}
//	else
//		printf("sudoku query failed!\n");
//	PL_close_query(query);
//}
//
//int main(int argc, char **argv){
//	PL_initialise(argc, argv);
//	consultFile("sudoku.pl");
//	solve("problem4.txt");
//	return 0;
//}
