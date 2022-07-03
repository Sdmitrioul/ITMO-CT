package spoon;

public class TooManyCommandsException extends Exception {
    private static String message = "Too many commands";

    public TooManyCommandsException() {
        super(message);
    }
}
