package spoon;

public class TooMuchMemoryException extends Exception {
    private static String message = "Too much memory, we haven't it enough";

    public TooMuchMemoryException() {
        super(message);
    }
}
