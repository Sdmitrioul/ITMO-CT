package c.skroba.rules;

import c.skroba.grammar.Expression;
import c.skroba.grammar.Variable;
import c.skroba.grammar.binary.Conjunction;
import c.skroba.grammar.binary.Disjunction;
import c.skroba.grammar.binary.Implication;
import c.skroba.grammar.calculus.All;
import c.skroba.grammar.calculus.Exist;
import c.skroba.grammar.unary.Negation;

import java.util.HashSet;

public class LogicAxioms {
	public static String getAxiom(final Expression expression)  {
		int result = firstAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		
		result = secondAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		
		result = thirdAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		
		result = fourthOrFifthAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		result = sixOrSevenAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		result = eightAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		result = ninthAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		result = tensAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		
		result = elevenAxiom(expression);
		
		if (result > 0) {
			return "" + result;
		}
		
		result = twelveAxioms(expression);
		
		if (result > 0) {
			return "" + result;
		}
		
		result = ArithmeticAxioms.getNinthAxiom(expression);
		
		if (result > 0) {
			return "A" + result;
		}
		
		return null;
	}
	
	//A -> (B -> A)
	private static int firstAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		Implication impl = (Implication) expression;
		
		if (!impl.right.isImplication()) {
			return -1;
		}
		
		Implication implSecond = (Implication) impl.right;
		
		return impl.left.equals(implSecond.right) ? 1 : -1;
	}
	
	// (A -> B) -> ((A -> B -> C) -> (A -> C))
	private static int secondAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.left.isImplication() || !impl.right.isImplication()) {
			return -1;
		}
		
		final Implication implLeft = (Implication) impl.left;
		final Implication implRight = (Implication) impl.right;
		
		if (!implRight.right.isImplication() || !implRight.left.isImplication()) {
			return -1;
		}
		
		final Implication implLast = (Implication) implRight.right;
		final Expression a = implLeft.left;
		final Expression b = implLeft.right;
		final Expression c = implLast.right;
		
		return implRight.left.equals(new Implication(a, new Implication(b, c))) ? 2 : -1;
	}
	
	// A -> (B -> (A & B))
	private static int thirdAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.right.isImplication()) {
			return -1;
		}
		
		final Implication implRight = (Implication) impl.right;
		
		if (!implRight.right.isConjunction()) {
			return -1;
		}
		
		final Expression a = impl.left;
		final Expression b = implRight.left;
		
		return implRight.right.equals(new Conjunction(a, b)) ? 3 : -1;
	}
	
	// (A & B) -> A
	// (A & B) -> B
	private static int fourthOrFifthAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.left.isConjunction()) {
			return -1;
		}
		
		final Conjunction conj = (Conjunction) impl.left;
		
		return conj.left.equals(impl.right) ? 4 : conj.right.equals(impl.right) ? 5 : -1;
	}
	
	// A -> (A | B)
	// B -> (A | B)
	private static int sixOrSevenAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.right.isDisjunction()) {
			return -1;
		}
		
		final Disjunction disj = (Disjunction) impl.right;
		
		return disj.left.equals(impl.left) ? 6 : disj.right.equals(impl.left) ? 7 : -1;
	}
	
	// (A -> C) -> ((B -> C) -> ((A | B) -> C))
	private static int eightAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.right.isImplication() || !impl.left.isImplication()) {
			return -1;
		}
		
		final Implication implLeft = (Implication) impl.left;
		final Expression a = implLeft.left;
		final Expression c = implLeft.right;
		
		final Implication implRight = (Implication) impl.right;
		
		if (!implRight.left.isImplication() || !implRight.right.isImplication()) {
			return -1;
		}
		
		final Implication implRightLeft = (Implication) implRight.left;
		final Expression b = implRightLeft.left;
		
		if (!implRightLeft.right.equals(c)) {
			return -1;
		}
		
		return implRight.right.equals(new Implication(new Disjunction(a, b), c)) ? 8 : -1;
	}
	
	// (A -> B) -> ((A -> !B) -> !A)
	private static int ninthAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.right.isImplication() || !impl.left.isImplication()) {
			return -1;
		}
		
		final Implication implLeft = (Implication) impl.left;
		final Expression a = implLeft.left;
		final Expression b = implLeft.right;
		
		final Implication implRight = (Implication) impl.right;
		
		if (!implRight.left.isImplication() || !implRight.right.isNegation()) {
			return -1;
		}
		
		final Implication middle = (Implication) implRight.left;
		final Negation neg = (Negation) implRight.right;
		
		if (!neg.expression.equals(a) || !middle.left.equals(a) || !middle.right.isNegation()) {
			return -1;
		}
		
		final Negation negMid = (Negation) middle.right;
		
		return negMid.expression.equals(b) ? 9 : -1;
	}
	
	// !!A -> A
	private static int tensAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.left.isNegation()) {
			return -1;
		}
		
		final Expression a = impl.right;
		final Negation neg = (Negation) impl.left;
		
		if (!neg.expression.isNegation()) {
			return -1;
		}
		
		final Negation secNeg = (Negation) neg.expression;
		
		return secNeg.expression.equals(a) ? 10 : -1;
	}
	
	// @x.A -> A(n)
	private static int elevenAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication implication = (Implication) expression;
		
		if (!implication.left.isAll()) {
			return -1;
		}
		
		final All left = (All) implication.left;
		final Expression right = implication.right;
		final Variable variable = left.variable;
		
		final Expression result = left.expression.diff(variable, right);
		
		if (result == null) {
			return -1;
		}
		
		if (left.expression.isFreeForVariables(variable, result.getFreeVariables(), new HashSet<>())) {
			return 11;
		}
		
		throw new CheckerException("variable " + variable + " is not free for term " + result + " in @-axiom.");
	}
	
	// A(n) -> ?x.A
	private static int twelveAxioms(final Expression expression) {
		if (!expression.isImplication()) {
			return -1;
		}
		
		final Implication implication = (Implication) expression;
		
		if (!implication.right.isOne()) {
			return -1;
		}
		
		final Exist right = (Exist) implication.right;
		final Expression left = implication.left;
		final Variable variable = right.variable;
		
		final Expression result = right.expression.diff(variable, left);
		
		if (result == null) {
			return -1;
		}
		
		if (right.expression.isFreeForVariables(variable, result.getFreeVariables(), new HashSet<>())) {
			return 12;
		}
		
		throw new CheckerException("variable " + variable + " is not free for term " + result + " in ?-axiom.");
	}
	
}
