package spoon;

import spoon.token.Token;
import spoon.token.Tokenizer;

import java.util.ArrayList;

public class Interpreter {
    private final static int MAXCOLOFCOMMANDS = 1000;
    private final static int MAXMEMORYSIZE = 10;
    private final int[] inputInformation;

    private static int[] arr = new int[MAXMEMORYSIZE];
    private String returningString;
    private Tokenizer tokens;

    public Interpreter(int[] inputInformation, Tokenizer tokens) {
        this.inputInformation = inputInformation;
        this.tokens = tokens;
        try {
            interpreter();
        } catch (TooManyCommandsException e) {
            System.out.println(e.getMessage());
        } catch (TooMuchMemoryException e) {
            System.out.println(e.getMessage());
        } catch (MemIndexException e) {
            System.out.println(e.getMessage());
        }
    }

    private void interpreter() throws TooManyCommandsException, TooMuchMemoryException, MemIndexException {
        ArrayList<Integer> list = new ArrayList<>();
        StringBuilder stringBuilder = new StringBuilder();
        int memPoint = 0;
        int inputPoint = 0;
        if (tokens.length() > MAXCOLOFCOMMANDS) {
            throw new TooManyCommandsException();
        } else {
            while (tokens.hasNext()) {
                Token token = tokens.next();
                switch (token.getType()) {
                    case NEXT:
                        memPoint++;
                        if (memPoint > MAXMEMORYSIZE) {
                            throw new TooMuchMemoryException();
                        }
                        break;
                    case PREV:
                        memPoint--;
                        if (memPoint < 0) {
                            throw new MemIndexException();
                        }
                        break;
                    case PLUS:
                        if (arr[memPoint] + 1 > 255) {
                            arr[memPoint] = 0;
                        } else {
                            arr[memPoint]++;
                        }
                        break;
                    case MINUS:
                        if (arr[memPoint] - 1 < 0) {
                            arr[memPoint] = 255;
                        } else {
                            arr[memPoint]--;
                        }
                        break;
                    case READ:
                        arr[memPoint] = inputInformation[inputPoint];
                        inputPoint++;
                        break;
                    case WRITE:
                        stringBuilder.append((char) arr[memPoint]);
                        break;
                    case BEGIN_WHILE:
                        list.add(tokens.getCurr() - 1);
                        break;
                    case END_WHILE:
                        if (arr[memPoint] > 0) {
                            tokens.getBack(list.remove(list.size() - 1));
                        }
                        break;
                }
            }
        }
        returningString = stringBuilder.toString();
    }

    public String getReturningString() {
        return returningString;
    }
}
