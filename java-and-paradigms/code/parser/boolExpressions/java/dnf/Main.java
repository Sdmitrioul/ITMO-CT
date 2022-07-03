package dnf;

import dnf.makeDNF.DNFform;
import dnf.makeDNF.Form;
import dnf.parser.*;

public class Main {
    public static void main(String[] args) {
        //String string = "(a & b) | ~c & a";
        String string = args[0];
        Parser tester = new ExpressionParser();
        Form dnFform = new DNFform(tester.parse(string), tester.getVariables());
        System.out.println(dnFform.getForm());
    }
}
