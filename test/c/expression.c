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

	// p6 expression
	i<a+b;
	j>=a;

	// p7 expression
	a==b;
	c!=d;

	// p8 expression
	a & b & c;

	// p9 expression
	a ^ b;

	// p10 expression
	a | b;

	// p11 expression, &&
	a && b;

	// p12 expression
	a || b;

	// p13 expression
	a ? b : c;

	// p14
	a = b+c;
	a += 1;
	a -= 2;
	a *= 3;
	a /= 4;
	a %= 5;
	a <<= 6;
	a >>= 7;
	a &= 8;
	a |= 9;
	a ^= 10;
}