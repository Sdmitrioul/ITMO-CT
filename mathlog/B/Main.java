import b.skroba.Checker;
import b.skroba.Proofing;
import b.skroba.Reader;
import b.skroba.WriterData;
import b.skroba.grammar.Expression;
import b.skroba.utils.Pair;
import b.skroba.utils.exception.AxiomException;

import java.io.IOException;
import java.util.List;

public class Main {
	public static void main(String[] args) {
		final Pair<Proofing, List<Expression>> input = Reader.readInput();
		try (final WriterData writerData = new WriterData()) {
			assert input != null;
			writerData.writeData(Checker.check(input), input.first.hypothesis);
		} catch (IOException ex) {
			System.out.println("Exception while writing!");
		} catch (AxiomException ex) {
			System.out.println(ex.getMessage());
			System.out.flush();
		}
	}
}
