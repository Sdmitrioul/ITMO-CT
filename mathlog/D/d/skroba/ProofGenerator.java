package d.skroba;

import d.skroba.grammar.Expression;
import d.skroba.grammar.Predicate;
import d.skroba.grammar.Variable;
import d.skroba.grammar.Zero;
import d.skroba.grammar.binary.Equal;
import d.skroba.grammar.binary.Implication;
import d.skroba.grammar.binary.Multiplication;
import d.skroba.grammar.binary.Sum;
import d.skroba.grammar.calculus.Exist;
import d.skroba.grammar.unary.Increment;
import d.skroba.grammar.unary.Number;
import d.skroba.util.Writer;


public class ProofGenerator implements AutoCloseable {
	private static final Expression ZERO = new Zero();
	private static final Expression ONE = Number.getNumber(1);
	private static final Expression TWO = Number.getNumber(2);
	private static final Expression TRUE = new Implication(new Predicate("A"), new Implication(new Predicate("A"), new Predicate("A")));
	private final Writer writer;
	
	public ProofGenerator(Writer writer) {
		this.writer = writer;
	}
	
	public void generate(int number) {
		final Expression gl = new Exist(new Variable('x'), equality(Number.getNumber(number), mul(TWO, new Variable('x'))));
		writer.write("|-" + gl);
		initWriting();
		if (number > 0) {
			generate(number, number / 2);
		} else {
			writer.write(seventhAxiom(TWO).toString());
			reverseAB(mul(2, 0), ZERO);
		}
		writer.write(impl(equality(num(number), mul(TWO, num(number / 2))), gl).toString());
		writer.write(gl.toString());
	}
	
	private void initWriting() {
		writer.write("(A->A->A)");
		writer.write("(a=b->(a)'=(b)')");
		writer.write("(a=b->(a)'=(b)')->(A->A->A)->(a=b->(a)'=(b)')");
		writer.write("(A->A->A)->(a=b->(a)'=(b)')");
		writer.write("(A->A->A)->@a.(a=b->(a)'=(b)')");
		writer.write("(A->A->A)->@b.@a.(a=b->(a)'=(b)')");
		writer.write("@b.@a.(a=b->(a)'=(b)')");
		writer.write("(a=b->a=c->b=c)");
		writer.write("(a=b->a=c->b=c)->(A->A->A)->(a=b->a=c->b=c)");
		writer.write("(A->A->A)->(a=b->a=c->b=c)");
		writer.write("(A->A->A)->@a.(a=b->a=c->b=c)");
		writer.write("(A->A->A)->@b.@a.(a=b->a=c->b=c)");
		writer.write("(A->A->A)->@c.@b.@a.(a=b->a=c->b=c)");
		writer.write("@c.@b.@a.(a=b->a=c->b=c)");
		writer.write("((a)+(b)'=((a)+(b))')");
		writer.write("((a)+(b)'=((a)+(b))')->(A->A->A)->((a)+(b)'=((a)+(b))')");
		writer.write("(A->A->A)->((a)+(b)'=((a)+(b))')");
		writer.write("(A->A->A)->@a.((a)+(b)'=((a)+(b))')");
		writer.write("(A->A->A)->@b.@a.((a)+(b)'=((a)+(b))')");
		writer.write("(@b.@a.((a)+(b)')=((a)+(b))')");
		writer.write("((a)+0=(a))");
		writer.write("((a)+0=(a))->(A->A->A)->((a)+0=(a))");
		writer.write("(A->A->A)->((a)+0=(a))");
		writer.write("(A->A->A)->@a.((a)+0=(a))");
		writer.write("@a.((a)+0=(a))");
		writer.write("((a)*0=(0))");
		writer.write("((a)*0=(0))->(A->A->A)->((a)*0=(0))");
		writer.write("(A->A->A)->((a)*0=(0))");
		writer.write("(A->A->A)->@a.((a)*0=(0))");
		writer.write("@a.((a)*0=(0))");
		writer.write("((a)*(b)'=(a)*(b)+(a))");
		writer.write("((a)*(b)'=(a)*(b)+(a))->(A->A->A)->((a)*(b)'=(a)*(b)+(a))");
		writer.write("(A->A->A)->((a)*(b)'=(a)*(b)+(a))");
		writer.write("(A->A->A)->@a.((a)*(b)'=(a)*(b)+(a))");
		writer.write("(A->A->A)->@b.@a.((a)*(b)'=(a)*(b)+(a))");
		writer.write("@b.@a.((a)*(b)'=(a)*(b)+(a))");
	}
	
	private void generate(int big, int small) {
		if (small == 0) {
			final Expression equal = seventhAxiom(TWO);
			writer.write(equal.toString());
			return;
		};
		
		writer.write(eightAxiom(2, small).toString());
		writer.write(fifthAxiom(mul(2, small - 1), num(1)).toString());
		writer.write(firstAxiom(num(big - 1), sum(mul(2, small - 1), num(1))).toString());
		writer.write(fifthAxiom(mul(2, small - 1), ZERO).toString());
		writer.write(firstAxiom(num(big - 2), sum(mul(2, small - 1), ZERO)).toString());
		writer.write(sixAxiom(mul(2, small - 1)).toString());
		
		generate(big - 2, (big - 2) / 2);
		
		reverseAB(sum(mul(2, small - 1), ZERO), mul(2, small -1));
		generateSecondAxiom(mul(2, small -1), num(big - 2), sum(mul(2, small - 1), ZERO));
		reverseAB(num(big - 1), inc(sum(mul(2, small - 1), ZERO)));
		reverseAB(sum(mul(2,small - 1), ONE), inc(sum(mul(2, small - 1), ZERO)));
		generateSecondAxiom(inc(sum(mul(2, small - 1), ZERO)), num(big - 1), sum(mul(2,small - 1), ONE));
		reverseAB(num(big), inc(sum(mul(2,small - 1), ONE)));
		reverseAB(sum(mul(2, small - 1), TWO), inc(sum(mul(2,small - 1), ONE)));
		generateSecondAxiom(inc(sum(mul(2,small - 1), ONE)), num(big), sum(mul(2, small - 1), TWO));
		reverseAB(num(big), sum(mul(2, small - 1), TWO));
		reverseAB(mul(2, small), sum(mul(2, small - 1), TWO));
		generateSecondAxiom(sum(mul(2, small - 1), TWO), num(big), mul(2, small));
		reverseAB(num(big), mul(2, small));
	}
	
	private void generateSecondAxiom(final Expression a, final Expression b, final Expression c) {
		final Expression ab = equality(a,b);
		final Expression ac = equality(a, c);
		final Expression bc = equality(b, c);
		final Expression acbc = new Implication(ac, bc);
		writer.write(ab.toString());
		writer.write(ac.toString());
		writer.write(secondAxiom(a, b, c).toString());
		writer.write(acbc.toString());
		writer.write(bc.toString());
	}
	
	private void reverseAB(final Expression a, final Expression b) {
		final Expression ab = equality(a, b);
		final Expression aa = equality(a, a);
		writer.write(ab.toString());
		writer.write(secondAxiom(a, b, a).toString());
		writer.write(impl(aa, equality(b, a)).toString());
		generateAA(a);
		writer.write(equality(b, a).toString());
	}
	
	private void generateAA(final Expression expression) {
		final Expression result = equality(expression, expression);
		final Expression pre = sixAxiom(expression);
		final Expression impl =impl(pre, result);
		writer.write(pre.toString());
		writer.write(secondAxiom(sum(expression, ZERO), expression, expression).toString());
		writer.write(impl.toString());
		writer.write(result.toString());
	}
	
	private Expression eightAxiom(int a, int b) {
		return eightAxiom(num(a), num(b - 1));
	}
	
	private Expression equality(final Expression a, final Expression b) {
		return new Equal(a, b);
	}
	
	private Expression equality(final  Expression a) {
		return equality(a, a);
	}
	
	private Expression inc(final int a) {
		return inc(num(a));
	}
	
	private Expression inc(final Expression a) {
		return new Increment(a);
	}
	
	private Expression sum(final int a, final int b) {
		return sum(num(a), num(b));
	}
	
	private Expression sum(final Expression a, final Expression b) {
		return new Sum(a, b);
	}
	
	private Expression mul(final int a, final int b) {
		return mul(num(a), num(b));
	}
	
	private Expression mul(final Expression a, final Expression b) {
		return new Multiplication(a, b);
	}
	
	private Expression impl(final int a, final int b) {
		return impl(num(a), num(b));
	}
	
	private Expression impl(final Expression a, final Expression b) {
		return new Implication(a, b);
	}
	
	private Expression num(final int i) {
		return Number.getNumber(i);
	}
	
	private Expression eightAxiom(final Expression a, final Expression b) {
		writer.write("(@b.@a.((a)*(b)'=(a)*(b)+(a)))->@a.((a)*(" + b + ")'=(a)*(" + b + ")+(a))");
		writer.write("@a.((a)*(" + b + ")'=(a)*(" + b + ")+(a))");
		
		final Expression res = equality(mul(a, inc(b)), sum(mul(a, b), a));
		
		writer.write("(@a.((a)*(" + b + ")'=(a)*(" + b + ")+(a)))->" + res);
		return res;
	}
	
	private Expression seventhAxiom(final Expression a) {
		final Expression res = equality(mul(a, ZERO), ZERO);
		
		writer.write("(@a.((a)*0=(0)))->" + res);
		return res;
	}
	
	private Expression sixAxiom(final Expression a) {
		final Expression res = equality(sum(a, ZERO), a);
		
		writer.write("(@a.((a)+0=(a)))->" + res);
		return res;
	}
	
	private Expression fifthAxiom(final Expression a, final Expression b) {
		writer.write("(@b.@a.((a)+(b)'=((a)+(b))'))->@a.((a)+(" + b + ")'=((a)+(" + b + "))')");
		writer.write("(@a.((a)+(" + b + ")')=((a)+(" + b + "))')");
		
		final Expression res = equality(sum(a, inc(b)), inc(sum(a, b)));
		
		writer.write("(@a.((a)+(" + b + ")'=((a)+(" + b + "))'))->" + res);
		return res;
	}
	
	private Expression secondAxiom(final Expression a, final Expression b, final Expression c) {
		writer.write("(@c.@b.@a.(a=b->a=c->b=c))->@b.@a.(a=b->a=" + c + "->b=" + c +")");
		writer.write("@b.@a.(a=b->a=" + c + "->b=" + c +")");
		writer.write("(@b.@a.(a=b->a=" + c + "->b=" + c +"))->@a.(a=" + b + "->a="+c + "->" + b + "=" + c + ")");
		writer.write("@a.(a=" + b + "->a="+c + "->" + b + "=" + c + ")");
		
		final Expression res = impl(equality(a, b), impl(equality(a, c), equality(b, c)));
		
		writer.write("(@a.(a=" + b + "->a="+c + "->" + b + "=" + c + "))->" + res);
		return res;
	}
	
	private Expression firstAxiom(final Expression a, final Expression b) {
		writer.write("(@b.@a.(a=b->(a)'=(b)'))->@a.(a=" + b + "->(a)'=(" + b + ")')");
		writer.write("@a.(a=" + b + "->(a)'=(" + b + ")')");
		final Expression res = impl(equality(a, b), equality(inc(a), inc(b)));
		writer.write("(@a.(a=" + b + "->(a)'=(" + b + ")'))->" + res);
		return res;
	}
	
	@Override
	public void close() {
		writer.close();
	}
}
