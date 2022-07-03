package csv;

public class Main {
    public static void main(String[] args) {
        //csv.CSV csv = new csv.CSV("CSVexample.csv");
        //csv.TSV tsv = new csv.TSV("TSVexample.tsv");
        //csv.SSV ssv = new csv.SSV("SSVexample.ssv");
        for (int i = 0; i < args.length; i++) {
            if (args[i].contains(".csv")) {
                CSV csv = new CSV(args[i]);
            } else if (args[i].contains(".ssv")) {
                SSV ssv = new SSV(args[i]);
            } else {
                TSV tsv = new TSV(args[i]);
            }
        }
    }
}
