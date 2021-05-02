int fib(int n) {
   if(n <= 1){
      return n;
   } else {
      return (fib(n-1) + fib(n-2));
   }
}

void main(){
	print(fib(6));
	
	fun (int -> int) fac = lambda(int a) : int {
		if (a==0)
			return 1;
		return a * fac(a-1);
	};
	
	print(fac(4));
}
