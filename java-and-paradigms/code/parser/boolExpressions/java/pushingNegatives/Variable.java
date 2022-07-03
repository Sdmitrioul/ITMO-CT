package pushingNegatives;

public class Variable implements Expression {
    private char name;

    public Variable(char name) {
        this.name = name;
    }

    @Override
    public boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j,
                            boolean k, boolean l, boolean m, boolean n, boolean o, boolean p, boolean q, boolean r, boolean s, boolean t,
                            boolean u, boolean v, boolean w, boolean x, boolean y, boolean z) {
        switch (name) {
            case 'a':
                return a;
            case 'b':
                return b;
            case 'c':
                return c;
            case 'd':
                return d;
            case 'e':
                return e;
            case 'f':
                return f;
            case 'g':
                return g;
            case 'h':
                return h;
            case 'i':
                return i;
            case 'j':
                return j;
            case 'k' :
                return k;
            case 'l':
                return l;
            case 'm':
                return m;
            case 'n':
                return n;
            case 'o':
                return o;
            case 'p':
                return p;
            case 'q':
                return q;
            case 'r':
                return r;
            case 's':
                return s;
            case 't':
                return t;
            case 'u':
                return u;
            case 'v' :
                return v;
            case 'w':
                return w;
            case 'x':
                return x;
            case 'y':
                return y;
            case 'z' :
                return z;
            default:
                return false;
        }
    }

    public char getName() {
        return name;
    }

    @Override
    public Expression doNegation() {
        return new Negation(this);
    }

    @Override
    public String toString() {
        return String.valueOf(name);
    }
}

