package kalah;

public class Cell {
    private int stones;
    private int number;

    public Cell(int number) {
        this.stones = 6;
        this.number = number;
    }

    public int getCol() {
        return stones;
    }

    public int getStones() {
        int st = stones;
        stones = 0;
        return st;
    }

    public void setStones(int stones) {
        this.stones++;
    }
}
