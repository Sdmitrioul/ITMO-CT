package skroba.lab1.utils.exception;

import java.io.IOException;

public class WrongDataException extends IOException {
    public WrongDataException(String message) {
        super(message);
    }
}
