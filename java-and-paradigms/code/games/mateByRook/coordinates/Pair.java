package mateByRook.coordinates;

public class Pair <Left, Right> {
    private final Left left;
    private final Right right;

    public Pair(Left left, Right right) {
        this.left = left;
        this.right = right;
    }

    public Left getLeft() {
        return left;
    }

    public Right getRight() {
        return right;
    }

    @Override
    public String toString() {
        return "Left=" + left +
                ", right=" + right;
    }
}
