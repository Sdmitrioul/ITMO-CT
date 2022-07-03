package csv;

public class SSV extends AbstractExpansion {
    public SSV(String filename) {
        treatment(';', '\'', filename);
    }
}
