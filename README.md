# Boomslang

## How to run

* Clone the project
* `cd Boomslang/app/`
* `make`
* `./interpreter` program (examples see below)

## Features

1. Types
    * int
    * bool
    * string
    * void
    * fun(type_1, type_2, ..., type_n -> returnType)
2. Variables, final variables, functional variables
3. Overshadow variables 
4. Basic arithmetics (Add/Sub, Mul/Div, Mod, Comparisons)
5. Assignment
6. if, else conditional blocks
7. while loop (break, continue, return early form function)
8. For loop (iterator is const, return early from function)
9. Functions
    * Recursion supported
    * Nested functions
    * Lambdas with recursion
    * Closure
10. Runtime error handling

## Program examples

```
int factorial(int a) {
	int r = 1;

	while (a > 0) {
	  r = r * a;
	  a = a - 1;
	}

	return r;
}

void main() {
    // 5 factorial is 120
    print("Factorial of 5 is " + factorial(5));
}
```

```
void testReturnInFor() {
    for (int i = 0 to 50) {
        if (i == 5) {
            return;
        }
        print(i);
    }
}

void main() {
    testReturnInFor();
}
```