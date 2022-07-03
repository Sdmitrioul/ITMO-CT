package e.skroba.parser;

import e.skroba.grammar.Expression;
import e.skroba.grammar.Predicate;
import e.skroba.grammar.Variable;
import e.skroba.grammar.Zero;
import e.skroba.grammar.binary.*;
import e.skroba.grammar.binary.*;
import e.skroba.grammar.calculus.All;
import e.skroba.grammar.calculus.Exist;
import e.skroba.grammar.unary.Increment;
import e.skroba.grammar.unary.Negation;
import e.skroba.parser.source.CharSource;
import e.skroba.util.Pair;

public class FormalArithmeticParser extends AbstractParser<Expression> {
	public FormalArithmeticParser(CharSource source) {
		super(source);
		nextChar();
	}
	
	public Expression parse() {
		final Expression result = parseExpression();
		
		if (eof()) {
			return result;
		}
		log(String.valueOf(ch));
		throw error("Expected end of file " + result);
	}
	
	private Expression parseExpression() {
		skipWhitespace();
		
		final Expression disjunction = parseDisjunction();
		
		skipWhitespace();
		
		if (test('-')) {
			expect('>');
			
			skipWhitespace();
			
			final Expression result = new Implication(disjunction, parseExpression());
			
			skipWhitespace();
			
			return result;
		}
		
		skipWhitespace();
		
		return disjunction;
	}
	
	private Expression parseDisjunction() {
		skipWhitespace();
		
		Expression conjunction = parseConjunction();
		
		while (test('|')) {
			skipWhitespace();
			conjunction = new Disjunction(conjunction, parseConjunction());
			skipWhitespace();
		}
		
		return conjunction;
	}
	
	private Expression parseConjunction() {
		skipWhitespace();
		
		Expression negation = parseUnary();
		
		while (test('&')) {
			skipWhitespace();
			negation = new Conjunction(negation, parseUnary());
			skipWhitespace();
		}
		
		return negation;
	}
	
	private Expression parseUnary() {
		skipWhitespace();
		
		if (test('@')) {
			final Pair<Variable, Expression> predicateCalculus = parsePredicateCalculus();
			return new All(predicateCalculus.first, predicateCalculus.second);
		}
		
		if (test('?')) {
			final Pair<Variable, Expression> predicateCalculus = parsePredicateCalculus();
			return new Exist(predicateCalculus.first, predicateCalculus.second);
		}
		
		if (test('!')) {
			final Expression result = parseUnary();
			skipWhitespace();
			return new Negation(result);
		}
		
		final Expression predicate = parsePredicate();
		
		if (predicate != null) {
			return predicate;
		}
		
		nextChar();
		
		if (test('(')) {
			final Expression result = parseExpression();
			expect(')');
			skipWhitespace();
			return result;
		}
		
		throw error("Expected Unary: got ch - " + ch);
	}
	
	private Expression parsePredicate() {
		skipWhitespace();
		if (between('A', 'Z')) {
			final Expression predicate = new Predicate(ch);
			nextChar();
			skipWhitespace();
			return predicate;
		}
		
		final Expression leftTerm = parseTerm();
		if (leftTerm == null) {
			return null;
		}
		skipWhitespace();
		expect('=');
		skipWhitespace();
		final Expression rightTerm = parseTerm();
		skipWhitespace();
		
		return new Equal(leftTerm, rightTerm);
	}
	
	private Expression parseTerm() {
		skipWhitespace();
		
		Expression summing = parseSumming();
		
		if (summing == null) {
			return null;
		}
		
		while (test('+')) {
			skipWhitespace();
			summing = new Sum(summing, parseSumming());
			skipWhitespace();
		}
		
		return summing;
	}
	
	private Expression parseSumming() {
		skipWhitespace();
		
		Expression multiplying = parseMultiplying();
		
		if (multiplying == null) {
			return null;
		}
		
		while (test('*')) {
			skipWhitespace();
			multiplying = new Multiplication(multiplying, parseMultiplying());
			skipWhitespace();
		}
		
		return multiplying;
	}
	
	private Expression parseMultiplying() {
		skipWhitespace();
		
		Expression result = parsePartOfMultiplying();
		
		if (result == null) {
			return null;
		}
		
		
		while (test('\'')) {
			result = new Increment(result);
			skipWhitespace();
		}
		
		return result;
	}
	
	private Expression parsePartOfMultiplying() {
		if (between('a', 'z')) {
			final Expression result = parseVariable();
			skipWhitespace();
			return result;
		}
		
		if (test('0')) {
			final Expression result = new Zero();
			skipWhitespace();
			return result;
		}
		
		if (test('(')) {
			final Expression result = parseTerm();
			if (result == null) {
				skipBackTo();
				return null;
			}
			if (test(')')) {
				skipWhitespace();
				return result;
			}
			skipBackTo();
		}
		
		return null;
	}
	
	private Pair<Variable, Expression> parsePredicateCalculus() {
		skipWhitespace();
		final Variable variable = parseVariable();
		skipWhitespace();
		expect('.');
		skipWhitespace();
		final Expression expression = parseExpression();
		skipWhitespace();
		return new Pair<>(variable, expression);
	}
	
	private Variable parseVariable() {
		if (!between('a', 'z')) {
			throw error("Expected variable(char from <a> to <z>, got: " + ch);
		}
		final Variable result = new Variable(ch);
		nextChar();
		skipWhitespace();
		return result;
	}
	
	private void skipWhitespace() {
		while (test(' ') || test('\r') || test('\n') || test('\t')) {
			//Ignore
		}
	}
	
	private void skipBackTo() {
		int counter = 0;
		prevChar();
		while (ch != '(' || counter != 0) {
			if (ch == ')') {
				counter++;
			}
			if (ch == '(') {
				counter--;
			}
			prevChar();
		}
	}
}
