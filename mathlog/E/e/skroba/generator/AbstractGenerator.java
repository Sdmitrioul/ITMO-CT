package e.skroba.generator;

import e.skroba.grammar.Expression;
import e.skroba.grammar.Predicate;
import e.skroba.grammar.Variable;
import e.skroba.grammar.Zero;
import e.skroba.grammar.binary.*;
import e.skroba.grammar.calculus.All;
import e.skroba.grammar.calculus.Exist;
import e.skroba.grammar.unary.Increment;
import e.skroba.grammar.unary.Negation;
import e.skroba.grammar.unary.Number;
import e.skroba.util.Writer;

import java.util.concurrent.ExecutionException;

public abstract class AbstractGenerator implements Generator {
	private static final Expression TRUE = impl(predicate('A'), impl(predicate('A'), predicate('A')));
	protected static final Expression ZERO = new Zero();
	protected static final Expression ONE = num(1);
	protected static final Expression TWO = num(2);
	protected static final Expression THREE = num(3);
	protected static final Variable X = var('x');
	
	protected final Writer writer;
	
	protected AbstractGenerator(Writer writer) {
		this.writer = writer;
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
		writer.write("(!((a)'=0))");
		writer.write("(!((a)'=0))->(A->A->A)->(!((a)'=0))");
		writer.write("(A->A->A)->(!((a)'=0))");
		writer.write("(A->A->A)->@a.(!((a)'=0))");
		writer.write("@a.(!((a)'=0))");
		writer.write("((a)'=(b)'->(a)=(b))");
		writer.write("((a)'=(b)'->(a)=(b))->(A->A->A)->((a)'=(b)'->(a)=(b))");
		writer.write("(A->A->A)->((a)'=(b)'->(a)=(b))");
		writer.write("(A->A->A)->@a.((a)'=(b)'->(a)=(b))");
		writer.write("(A->A->A)->@b.@a.((a)'=(b)'->(a)=(b))");
		writer.write("@b.@a.((a)'=(b)'->(a)=(b))");
	}
	
	protected abstract void generateFirstLine(int num);
	
	protected abstract void generateLogic(int numberFor);
	
	@Override
	public void generateProofing(int numberFor) {
		generateFirstLine(numberFor);
		initWriting();
		generateLogic(numberFor);
	}
	
	protected static Expression num(int num) {
		return Number.getNumber(num);
	}
	
	protected static Variable var(char ch) {
		return new Variable(ch);
	}
	
	protected static Expression predicate(char ch) {
		return new Predicate(ch);
	}
	
	protected static Expression inc(int a) {
		return inc(num(a));
	}
	
	protected Expression mul(final int a, final int b) {
		return mul(num(a), num(b));
	}
	
	protected static Expression inc(Expression a) {
		return new Increment(a);
	}
	
	protected static Expression impl(Expression a, Expression b) {
		return new Implication(a, b);
	}
	
	protected static Expression neg(Expression a) {
		return new Negation(a);
	}
	
	protected static Expression mul(Expression a, Expression b) {
		return new Multiplication(a, b);
	}
	
	protected static Expression sum(Expression a, Expression b) {
		return new Sum(a, b);
	}
	
	protected static Expression equality(Expression a, Expression b) {
		return new Equal(a, b);
	}
	
	protected static Expression exist(Variable x, Expression a) {
		return new Exist(x, a);
	}
	
	protected static Expression all(Variable x, Expression a) {
		return new All(x, a);
	}
	
	protected Expression eightAxiom(int a, int b) {
		return eightAxiom(num(a), num(b - 1));
	}
	
	protected Expression eightAxiom(final Expression a, final Expression b) {
		writer.write("(@b.@a.((a)*(b)'=(a)*(b)+(a)))->@a.((a)*(" + b + ")'=(a)*(" + b + ")+(a))");
		writer.write("@a.((a)*(" + b + ")'=(a)*(" + b + ")+(a))");
		
		final Expression res = equality(mul(a, inc(b)), sum(mul(a, b), a));
		
		writer.write("(@a.((a)*(" + b + ")'=(a)*(" + b + ")+(a)))->" + res);
		return res;
	}
	
	protected Expression seventhAxiom(final Expression a) {
		final Expression res = equality(mul(a, ZERO), ZERO);
		
		writer.write("(@a.((a)*0=(0)))->" + res);
		return res;
	}
	
	protected Expression sixAxiom(final Expression a) {
		final Expression res = equality(sum(a, ZERO), a);
		
		writer.write("(@a.((a)+0=(a)))->" + res);
		return res;
	}
	
	protected Expression fifthAxiom(final Expression a, final Expression b) {
		writer.write("(@b.@a.((a)+(b)'=((a)+(b))'))->@a.((a)+(" + b + ")'=((a)+(" + b + "))')");
		writer.write("(@a.((a)+(" + b + ")')=((a)+(" + b + "))')");
		
		final Expression res = equality(sum(a, inc(b)), inc(sum(a, b)));
		
		writer.write("(@a.((a)+(" + b + ")'=((a)+(" + b + "))'))->" + res);
		return res;
	}
	
	protected Expression fourthAxiom(final int a) {
		return fourthAxiom(num(a - 1));
	}
	
	protected Expression fourthAxiom(final Expression a) {
		final Expression res = neg(equality(inc(a), ZERO));
		writer.write("(@a.(!((a)'=0)))->" + res);
		return res;
	}
	
	protected Expression thirdAxiom(final Expression a, final Expression b) {
		final Expression res = impl(equality(inc(a), inc(b)), equality(a, b));
		writer.write("(@b.@a.((a)'=(b)'->(a)=(b)))->@a.((a)'=" + inc(b) + "->(a)=" + b +")");
		writer.write("(@a.((a)'=" + inc(b) + "->(a)=" + b +"))");
		writer.write("(@a.((a)'=" + inc(b) + "->(a)=" + b +"))->" + res);
		return res;
	}
	
	protected Expression secondAxiom(final Expression a, final Expression b, final Expression c) {
		writer.write("(@c.@b.@a.(a=b->a=c->b=c))->@b.@a.(a=b->a=" + c + "->b=" + c +")");
		writer.write("@b.@a.(a=b->a=" + c + "->b=" + c +")");
		writer.write("(@b.@a.(a=b->a=" + c + "->b=" + c +"))->@a.(a=" + b + "->a="+c + "->" + b + "=" + c + ")");
		writer.write("@a.(a=" + b + "->a="+c + "->" + b + "=" + c + ")");
		
		final Expression res = impl(equality(a, b), impl(equality(a, c), equality(b, c)));
		
		writer.write("(@a.(a=" + b + "->a="+c + "->" + b + "=" + c + "))->" + res);
		return res;
	}
	
	protected Expression firstAxiom(final Expression a, final Expression b) {
		writer.write("(@b.@a.(a=b->(a)'=(b)'))->@a.(a=" + b + "->(a)'=(" + b + ")')");
		writer.write("@a.(a=" + b + "->(a)'=(" + b + ")')");
		final Expression res = impl(equality(a, b), equality(inc(a), inc(b)));
		writer.write("(@a.(a=" + b + "->(a)'=(" + b + ")'))->" + res);
		return res;
	}
	
	protected static Expression firstLogicAxiom(final Expression a, final Expression b) {
		return impl(a, impl(b, a));
	}
	
	protected void generateSecondAxiom(final Expression a, final Expression b, final Expression c) {
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
	
	protected void reverseAB(final Expression a, final Expression b) {
		final Expression ab = equality(a, b);
		final Expression aa = equality(a, a);
		writer.write(ab.toString());
		writer.write(secondAxiom(a, b, a).toString());
		writer.write(impl(aa, equality(b, a)).toString());
		generateAA(a);
		writer.write(equality(b, a).toString());
	}
	
	protected void generateAA(final Expression expression) {
		final Expression result = equality(expression, expression);
		final Expression pre = sixAxiom(expression);
		final Expression impl =impl(pre, result);
		writer.write(pre.toString());
		writer.write(secondAxiom(sum(expression, ZERO), expression, expression).toString());
		writer.write(impl.toString());
		writer.write(result.toString());
	}
	
	protected void generateAll(final Expression expression) {
		writer.write(firstLogicAxiom(expression, TRUE));
		writer.write(impl(TRUE, expression));
		writer.write(impl(TRUE, all(X, expression)));
		writer.write(all(X, expression));
	}
	
	protected void generateFirstAxiomAndMPOnes(final Expression a, final Expression b) {
		writer.write(impl(a, impl(b, a)));
		writer.write(impl(b, a));
	}
	
	protected void generateSecondAxiomMP(final Expression a, final Expression b, final Expression c) {
		Expression left = impl(a, b);
		Expression mid = impl(a, impl(b, c));
		Expression right = impl(a, c);
		
		writer.write(impl(left, impl(mid, right)));
		writer.write(impl(mid, right));
		writer.write(right);
	}
	
	protected void generateNinthAxiomMP(final Expression a, final Expression b) {
		Expression left = impl(a, b);
		Expression mid = impl(a, neg(b));
		Expression right = neg(a);
		writer.write(impl(left, impl(mid, right)));
		writer.write(impl(mid, right));
		writer.write(right);
	}
	
	protected void generateThirdAxiomMP(final Expression a, final Expression b) {
		Expression conj = new Conjunction(a, b);
		Expression rightImpl = impl(b, conj);
		writer.write(impl(a, rightImpl));
		writer.write(rightImpl);
		writer.write(conj);
	}
	
	protected void generateInductionAxiomMP(final Expression zero, final Expression all, final Expression res) {
		writer.write(impl(new Conjunction(zero, all), res));
		writer.write(res);
	}
	
	protected static Expression inc(final Expression a, int i) {
		Expression res = a;
		for (int j = 0; j < i; j++) {
			res = inc(res);
		}
		return res;
	}
	
	@Override
	public void close() {
		writer.close();
	}
}
