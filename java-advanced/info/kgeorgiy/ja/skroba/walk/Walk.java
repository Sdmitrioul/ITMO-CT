package info.kgeorgiy.ja.skroba.walk;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Path;

public class Walk extends FileWalker {
    public static void main(String[] args) {
        if (validArguments(args)) {
            Walk walker = new Walk();
            walker.process(args[0], args[1]);
        } else {
            System.err.println("Format of launch: java Walk <input filename> <output filename>");
        }
    }

    protected void pathOperating(final Path path, final BufferedWriter outputWriter) throws IOException {
        final String hash = String.format("%016x", WriterPJWHash.hash(path));
        outputWriter.write(hash + " " + path.toString() + "\n");
    }
}
