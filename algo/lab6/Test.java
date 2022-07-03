public class Test {
	
	public static void main(String[] args) {
		Test test = new Test();
		test.gcd(442, 880, new Pair());
	}
	
	private long gcd(long a, long b, Pair pair) {
		if (a == 0) {
			pair.a = 0;
			pair.b = 1;
			return b;
		}
		
		Pair nPair = new Pair();
		long d = gcd(b%a, a, nPair);
		pair.a = nPair.b - (b / a) * nPair.a;
		pair.b = nPair.a;
		System.out.println(a + " " + b + " " + pair.a + " " + pair.b);
		return d;
	}
	
	private static class Pair {
		long a = Integer.MIN_VALUE;
		long b = Integer.MIN_VALUE;;
	}
}
