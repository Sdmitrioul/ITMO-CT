package knf;

public interface ToMiniString {
    default String toMiniString() {
        return toString();
    }
}
