package d.skroba.rules;

import d.skroba.grammar.Expression;
import d.skroba.grammar.Variable;
import d.skroba.grammar.Zero;
import d.skroba.grammar.binary.*;
import d.skroba.grammar.binary.*;
import d.skroba.grammar.calculus.All;
import d.skroba.grammar.unary.Increment;
import d.skroba.grammar.unary.Negation;

public class ArithmeticAxioms {
	public static String getAxiom(final Expression expression)  {
		int result = getFirstAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		
		result = getSecondAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		
		result = getThirdAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		result = getFourthAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		result = getFifthAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		result = getSixAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		result = getSevenAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		result = getEightAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		
		return null;
	}
	
	// a = b -> a' = b'
	private static int getFirstAxiom(final Expression  expression) {
		if (!expression.isImplication())  {
			return -1;
		}
		
		final Implication impl = (Implication)  expression;
		
		if (!impl.left.isEquality() || !impl.right.isEquality()) {
			return -1;
		}
		
		final Equal left = (Equal) impl.left;
		final Equal right = (Equal) impl.right;
		
		if (!right.left.isIncrement() || !right.right.isIncrement()) {
			return -1;
		}
		
		return impl.right.equals(new Equal(new Increment(left.left), new Increment(left.right))) ? 1 : -1;
  	}
  	
  	// a = b -> ( a = c -> b = c)
  	private static int getSecondAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication big = (Implication) expression;
		
		if (!big.left.isEquality() || !big.right.isImplication()) {
			return -1;
		}
		
		final Equal firstEqual = (Equal) big.left;
		final Implication smallImpl = (Implication) big.right;
		
		if (!smallImpl.left.isEquality() || !smallImpl.right.isEquality()) {
			return -1;
		}
		
		final Equal middleEqual = (Equal) smallImpl.left;
		
		if (!firstEqual.left.equals(middleEqual.left)) {
			return -1;
		}
		
		final  Equal right = (Equal) smallImpl.right;
		
		return right.left.equals(firstEqual.right) && right.right.equals(middleEqual.right) ? 2 : -1;
	}
	
	// a' = b' -> a = b
	private static int getThirdAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.left.isEquality() || !impl.right.isEquality()) {
			return -1;
		}
		
		final Equal left = (Equal) impl.left;
		final Equal right = (Equal) impl.right;
		
		if (!left.left.isIncrement() || !left.right.isIncrement()) {
			return -1;
		}
		
		return left.equals(new Equal(new Increment(right.left), new Increment(right.right))) ? 3 : -1;
	}
	
	// !(a' = 0)
	private static int getFourthAxiom(final Expression expression) {
		if (!expression.isNegation()) {
			return -1;
		}
		
		final Negation neg = (Negation) expression;
		
		if (!neg.expression.isEquality()) {
			return -1;
		}
		
		final Equal equal = (Equal) neg.expression;
		
		return equal.left.isIncrement()  && equal.right.isZero() ? 4 : -1;
	}
	
	// a + b' = (a + b)'
	private static int getSixAxiom(final Expression expression) {
		if (!expression.isEquality()) {
			return -1;
		}
		
		final Equal equal = (Equal) expression;
		
		if (!equal.left.isSum() || !equal.right.isIncrement()) {
			return -1;
		}
		
		final Sum sum = (Sum) equal.left;
		final Increment inc = (Increment) equal.right;
		
		if (!sum.right.isIncrement() || !inc.expression.isSum()) {
			return -1;
		}
		
		final Increment secInc = (Increment) sum.right;
		final Sum secondSum = (Sum) inc.expression;
		
		return sum.left.equals(secondSum.left) && secInc.expression.equals(secondSum.right) ? 6 : -1;
	}
	
	// a + 0 = a
	private static int getFifthAxiom(final Expression expression) {
		if (!expression.isEquality()) {
			return -1;
		}
		
		final Equal equal = (Equal) expression;
		
		if (!equal.left.isSum()) {
			return -1;
		}
		
		final Sum sum = (Sum) equal.left;
		
		boolean res = sum.right.isZero() && sum.left.equals(equal.right);
		return res ? 5 : -1;
	}
	
	// a * 0 = 0
	private static int getSevenAxiom(final Expression expression) {
		if (!expression.isEquality()) {
			return -1;
		}
		
		final Equal equal = (Equal) expression;
		
		if (!equal.left.isMul() || !equal.right.isZero()) {
			return -1;
		}
		
		final Multiplication mul = (Multiplication) equal.left;
		
		return mul.right.isZero() ? 7 : -1;
	}
	
	// a * b' = a * b + a
	private static int getEightAxiom(final Expression expression) {
		if (!expression.isEquality()) {
			return -1;
		}
		
		final Equal equal = (Equal) expression;
		
		if (!equal.left.isMul() || !equal.right.isSum()) {
			return -1;
		}
		
		final Multiplication mul = (Multiplication) equal.left;
		final Sum sum = (Sum) equal.right;
		
		if (!mul.right.isIncrement() || !sum.left.isMul() || !sum.right.equals(mul.left)) {
			return -1;
		}
		
		final Multiplication secondMul = (Multiplication) sum.left;
		final Increment inc = (Increment) mul.right;
		
		return mul.left.equals(secondMul.left) && inc.expression.equals(secondMul.right) ? 8 : -1;
	}
	
	// A(x = 0) & @x.(A -> A(x = x')) -> A
	protected static int getNinthAxiom(final Expression expression) {
		if (!expression.isImplication()){
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.left.isConjunction()) {
			return -1;
		}
		
		final Conjunction leftConj = (Conjunction) impl.left;
		
		if (!leftConj.right.isAll()) {
			return -1;
		}
		
		
		final All all = (All) leftConj.right;
		if (!all.expression.isImplication()) {
			return -1;
		}
		
		final Implication insImpl = (Implication) all.expression;
		final Expression a = impl.right;
		final Variable var = all.variable;
		
		return !insImpl.left.equals(a)
				|| !a.diff(var, leftConj.left).equals(new Zero())
				|| !a.diff(var, insImpl.right).equals(new Increment(var))
				|| !a.getFreeVariables().contains(var.name)
				? -1 : 9;
	}
}
