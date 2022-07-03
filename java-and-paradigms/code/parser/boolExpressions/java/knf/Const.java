package knf;

public class Const implements Expression {
    private boolean value;

    public Const(boolean value) {
        this.value = value;
    }

    @Override
    public boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j) {
        return value;
    }

    @Override
    public String toString() {
        if (value) {
            return "1";
        } else {
            return "0";
        }
    }
}
