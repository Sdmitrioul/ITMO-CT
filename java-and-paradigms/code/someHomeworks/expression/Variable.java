package expression;

public class Variable implements CommonExpression{
    private String name;

    public Variable(String name) {
        this.name = name;
    }

    public int evaluate(int x, int y, int z) {
        switch (name) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                return -1;
        }
    }

    @Override
    public String toString() {
        return name;
    }
}
