package pushingNegatives;

public interface ToMiniString {
    default String toMiniString() {
        return toString();
    }
}
