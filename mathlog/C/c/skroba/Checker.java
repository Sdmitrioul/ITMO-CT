package c.skroba;

import c.skroba.grammar.Empty;
import c.skroba.grammar.Expression;
import c.skroba.rules.CheckerException;
import c.skroba.util.Reader;
import c.skroba.util.Writer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Checker implements AutoCloseable {
	private final Reader reader;
	private final Writer writer;
	
	public Checker(Reader reader, Writer writer) {
		this.reader = reader;
		this.writer = writer;
	}
	
	public void run() {
		final Expression proofing = readProofing();
		
		writer.write("|-" + proofing);
		
		final List<Expression> proofed = new ArrayList<>();
		final Map<Integer, Integer> hashesProofed = new HashMap<>();
		int index = 0;
		Expression current = null;
		
		while (reader.hasNext()) {
			index++;
			current = nextExpression();
			String result;
			
			try {
				result = FormalArithmetic.checkExpression(current, proofed, hashesProofed);
			} catch (CheckerException ex) {
				writer.write("Expression " + index + ": " + ex.getMessage());
				return;
			}
			
			if (result == null) {
				writer.write("Expression " + index + " is not proved.");
				return;
			}
			
			writer.write("[" + index + ". " + result);
			proofed.add(current);
			hashesProofed.putIfAbsent(current.hashCode(), index);
		}
		
		if (current == null) {
			current = new Empty();
		}
		
		assert proofing != null;
		if (!proofing.equals(current)) {
			writer.write("The proof proves different expression.");
		}
	}
	
	private Expression readProofing() {
		if (reader.hasNext()) {
			return FormalArithmetic.parse(reader.next().split("\\|-")[1]);
		}
		return null;
	}
	
	private Expression nextExpression() {
		return FormalArithmetic.parse(reader.next());
	}
	
	@Override
	public void close() {
		reader.close();
		writer.close();
	}
}
