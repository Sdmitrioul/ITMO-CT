package kalah;

public class KalaBoard implements Board, Position {
    private Cell[][] cells;
    private int turn;
    private int emptyCells;
    private int bucket1;
    private int bucket2;

    public KalaBoard() {
        this.cells = new Cell[2][6];
        for (int i = 0; i < 6; i++) {
            cells[0][i] = new Cell(1);
            cells[1][i] = new Cell(2);
        }
        turn = 0;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }
        int stones = cells[turn][move.getRow()].getStones();
        int row = move.getRow();
        int detx = 0;
        int str = 0;
        if (turn == 0) {
            detx = -1;
            str = 0;
        } else {
            detx = 1;
            str = 1;
        }
        row += detx;
        for (int i = 0; i < stones; i++) {
            if (row < 0 && turn == 0) {
                bucket1++;
                detx = 1;
                row += detx;
                str = 1;
                continue;
            } else {
                if (row > 5 && turn == 1) {
                    bucket2++;
                    detx = -1;
                    row += detx;
                    str = 0;
                    continue;
                } else {
                    if (i != stones - 1) {
                        cells[str][row].setStones(1);
                        row +=detx;
                        continue;
                    } else {
                        if (cells[str][row].getCol() == 0 && turn == str) {
                            if (turn == 0) {
                                bucket1 += 1 + cells[1][row].getStones();
                            } else {
                                bucket2 += 1 + cells[0][row].getStones();
                            }
                            continue;
                        } else {
                            cells[str][row].setStones(1);
                            row +=detx;
                            continue;
                        }
                    }
                }
            }
        }

        if (bucket1 + bucket2 == 72) {
            if (bucket1 < bucket2) {
                if (turn == 0) {
                    return Result.LOSE;
                } else {
                    return  Result.WIN;
                }
            } else {
                if (turn == 0) {
                    return Result.WIN;
                } else {
                    return  Result.LOSE;
                }
            }
        }
        turn = turn == 0 ? 1 : 0;
        return Result.UNKNOWN;
    }

    @Override
    public int getCell() {
        return turn;
    }

    @Override
    public Position getPosition() {
        return this;
    }


    @Override
    public boolean isValid(Move move) {
        return move.getRow() < 6 && cells[move.getValue()][move.getRow()].getCol() > 0;
    }

    @Override
    public int getCell(int row, int turn) {
        return cells[turn][row].getCol();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("");
        for (int i = 0; i < 6; i++) {
            sb.append(i).append(" |");
        }
        sb.append("\n");
        for (int i = 0; i < 6; i++) {
            sb.append("__|");
        }
        sb.append("\n");
        for (int i = 0; i < 6; i++) {
            if (cells[0][i].getCol() < 10) {
                sb.append(cells[0][i].getCol()).append(" |");
            } else {
                sb.append(cells[0][i].getCol()).append("|");
            }
        }
        sb.append("\n");
        for (int i = 0; i < 6; i++) {
            if (cells[1][i].getCol() < 10) {
                sb.append(cells[1][i].getCol()).append(" |");
            } else {
                sb.append(cells[1][i].getCol()).append("|");
            }
        }
        sb.append("\n");
        sb.append("Bucket first player: ").append(bucket1).append(";");
        sb.append("\n");
        sb.append("Bucket second player: ").append(bucket2).append(".");
        return sb.toString();
    }
}
