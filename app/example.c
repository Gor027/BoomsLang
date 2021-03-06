void testRecursiveLambda() {
    fun(int -> int) fib = lambda(int a) : int {
        if (a <= 1) {
            return a;
        }

        return fib(a-1) + fib(a-2);
    };

    print("10th Fibonacci number is " + fib(10));
}

fun(void -> void) testClosure() {
    string name = "Chrome";

    void printName() {
        print(name);
    }

    return printName;
}

int main() {
    fun(int -> int) additionBy(int adder) {
        return lambda(int x) : int {
            return x + adder;
        };
    };

    fun(int -> int) addBy2 = additionBy(2);
    fun(int -> int) addBy5 = additionBy(5);

    print("Result of 5 + 2 is " + addBy2(5));
    print("Result of 5 + 5 is " + addBy5(5));

    testRecursiveLambda();
    fun(void -> void) namePrinter = testClosure();

    namePrinter();

    return 0;
}