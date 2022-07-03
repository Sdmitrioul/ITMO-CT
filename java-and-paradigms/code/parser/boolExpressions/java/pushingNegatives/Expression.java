package pushingNegatives;

public interface Expression extends ToMiniString {
    boolean evaluate(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h, boolean i, boolean j,
                     boolean k, boolean l, boolean m, boolean n, boolean o, boolean p, boolean q, boolean r, boolean s, boolean t,
                     boolean u, boolean v, boolean w, boolean x, boolean y, boolean z);
    Expression doNegation();
}
