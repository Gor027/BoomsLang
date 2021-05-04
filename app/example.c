int factorial(int a) {
	int r;
	int n;

	r = 1;
	n = a;

	while (n > 0) {
	  r = r * n;
	  n = n - 1;
	}

	return r;
}

void main() {
    print(factorial(5));
}
