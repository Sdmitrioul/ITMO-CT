package csv;

public class CSV extends AbstractExpansion {
    public CSV(String filename) {
        treatment(',', '\"' , filename);
    }
}
