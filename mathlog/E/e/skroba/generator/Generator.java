package e.skroba.generator;

public interface Generator extends AutoCloseable {
	void generateProofing(int numberFor);
}
