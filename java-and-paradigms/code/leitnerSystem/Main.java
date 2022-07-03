package leitnerSystem;

import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        if (args.length != 0) {
            LeitnerSystem leitnerSystem = new LeitnerSystem(args[0]);
        } else {
            LeitnerSystem leitnerSystem = new LeitnerSystem("");
        }
    }
}
