package csv;

public class TSV extends AbstractExpansion {
    public TSV(String filename) {
        treatment('\t', ' ', filename);
    }
}
