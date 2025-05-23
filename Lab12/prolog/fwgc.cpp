#include <stdio.h>
#include <SWI-Prolog.h>

void consultFile(const char* fileName) {
	predicate_t p_consult = PL_predicate("consult", 1, "database");
	term_t t = PL_new_term_ref();
	PL_put_string_chars(t, fileName);
	PL_call_predicate(NULL, 0, p_consult, t);
}

void solve() {
	predicate_t p_puzzle = PL_predicate("solve_puzzle", 0, "database");
	term_t t = PL_new_term_ref();
	qid_t query = PL_open_query(NULL, PL_Q_NORMAL, p_puzzle, t);
	int result;
	bool foundSolution{ false };
	do {
		result = PL_next_solution(query);
		if (!result)
			break;
		foundSolution = true;
		printf("\n");
	} while (result);
	if (!foundSolution)
		printf("Query failed!\n");
	PL_close_query(query);
}

int main(int argc, char** argv) {
	PL_initialise(argc, argv);
	consultFile("fwgc.pl");
	solve();

	return 0;
}