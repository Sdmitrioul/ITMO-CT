package pushingNegatives;

public class Const implements Expression {
    private boolean value;

    public Const(boolean value) {
        this.value = value;
    }

    @Override
    public boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j,
                            boolean k, boolean l, boolean m, boolean n, boolean o, boolean p, boolean q, boolean r, boolean s, boolean t,
                            boolean u, boolean v, boolean w, boolean x, boolean y, boolean z) {
        return value;
    }

    @Override
    public Expression doNegation() {
        return new Const(!value);
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
