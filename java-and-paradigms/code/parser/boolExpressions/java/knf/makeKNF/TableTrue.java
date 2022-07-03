package knf.makeKNF;

import knf.Expression;

public class TableTrue {
    private boolean[][] tableTrue;
    private Expression expression;

    public TableTrue(Expression expression, int columns) {
        this.expression = expression;
        int raws = (int) Math.pow( 2, columns);
        tableTrue = new boolean[raws][11];
        makeTableTrue(raws, columns);
    }

    public boolean[][] getTableTrue() {
        return tableTrue;
    }

    private void makeTableTrue(int raws, int columns) {
        for (int i = 0; i < raws; i++) {
            StringBuilder sb = new StringBuilder();
            String binary = Integer.toBinaryString(i);
            sb.append("0".repeat(Math.max(0, columns - binary.length())));
            sb.append(binary);
            sb.append("0".repeat(Math.max(0, 10 - columns)));
            for (int j = 0; j < 10; j++) {
                tableTrue[i][j] = sb.charAt(j) == '1';
            }
            tableTrue[i][10] = expression.evaluate(tableTrue[i][0], tableTrue[i][1], tableTrue[i][2], tableTrue[i][3], tableTrue[i][4],
                    tableTrue[i][5], tableTrue[i][6], tableTrue[i][7], tableTrue[i][8], tableTrue[i][9]);
        }
    }
}
