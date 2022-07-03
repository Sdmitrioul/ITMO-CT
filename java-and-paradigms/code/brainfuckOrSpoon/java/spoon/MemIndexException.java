package spoon;

public class MemIndexException extends Exception {
    private static String message = "Array index out of bound exception";

    public MemIndexException() {
        super(message);
    }
}
