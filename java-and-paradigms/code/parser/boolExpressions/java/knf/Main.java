package knf;

import knf.makeKNF.*;
import knf.parser.*;

public class Main {
    public static void main(String[] args) {
        //String string = "(a & b) | ~c & a";
        String string = args[0];
        Parser tester = new ExpressionParser();
        Form knFform = new KNFform(tester.parse(string), tester.getVariables());
        System.out.println(knFform.getForm());
    }
}
