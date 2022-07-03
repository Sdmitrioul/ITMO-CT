import c.skroba.Checker;
import c.skroba.util.Reader;
import c.skroba.util.Writer;

public class Main {
	public static void main(String[] args) {
		try (final Checker checker = new Checker(new Reader("input"), new Writer("output"))) {
			checker.run();
		}
	}
	
}
