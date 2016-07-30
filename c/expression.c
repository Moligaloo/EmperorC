#include <stdio.h>

int main(int argc, char **argv)
{
	// p0 expression
	a;
	"hello";
	123;

	// p1 expression
	a.b->c[0](); 
	i++;
	j--;

	// p2 expression
	++(x);
	--x;
	-x;
	-1;
	!a;	
	(int)x;
	*(a);
	&var;
	sizeof(a);
	sizeof int;

	// p3 expression
	(a*b%c);
	(x%y*z);
	(a%b);

	// p4 expression
	a+b*c-d;

	// p5 expression
	a << 2;
	a+1 >> 3;

	i<a+b;
	j>=a;
}