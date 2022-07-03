package a.skroba.utils.parser;

import a.skroba.grammar.Expression;
import a.skroba.grammar.Variable;
import a.skroba.grammar.binary.Conjunction;
import a.skroba.grammar.binary.Disjunction;
import a.skroba.grammar.binary.Implication;
import a.skroba.grammar.unary.Negation;
import a.skroba.utils.AbstractParser;
import a.skroba.utils.source.CharSource;

public class LogicExpressionParser extends AbstractParser {
	public LogicExpressionParser(CharSource source) {
		super(source);
		nextChar();
	}
	
	public Expression parse() {
		final Expression result = parseExpression();
		
		if (eof()) {
			return result;
		}
		log(String.valueOf(ch));
		throw error("Expected end of file " + result.toSimpleTreeGrammar());
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
		
		Expression negation = parseNegation();
		
		while (test('&')) {
			skipWhitespace();
			negation = new Conjunction(negation, parseNegation());
			skipWhitespace();
		}
		
		return negation;
	}
	
	private Expression parseNegation() {
		skipWhitespace();
		
		if (test('!')) {
			final Expression result = parseNegation();
			skipWhitespace();
			return new Negation(result);
		}
		
		if (between('A', 'Z')) {
			final Expression result = parseVariable();
			skipWhitespace();
			return result;
		}
		
		if (test('(')) {
			final Expression result = parseExpression();
			expect(')');
			skipWhitespace();
			return result;
		}
		
		throw error("Expected one of: !⟨Negation⟩, ⟨Variable⟩ or \"(\"⟨Expression⟩\")\", but have: " + ch);
	}
	
	private Expression parseVariable() {
		skipWhitespace();
		
		final StringBuilder variable = new StringBuilder();
		
		while (between('A', 'Z') || between('0', '9') || check('\'')) {
			variable.append(ch);
			nextChar();
		}
		
		skipWhitespace();
		
		return new Variable(variable.toString());
	}
	
	private void skipWhitespace() {
		while (test(' ') || test('\r') || test('\n') || test('\t')) {
			//Ignore
		}
	}
}
