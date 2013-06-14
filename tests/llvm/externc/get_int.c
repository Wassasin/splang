#include <stdio.h>

int get_int(){
	printf("Please enter a number, 0 to end\n");

	int myInt;
	int result = scanf("%d", &myInt);

	if (result == EOF) return 0;
	if (result == 0) return 0;

	return myInt;
}
