package dnf;

public interface ToMiniString {
    default String toMiniString() {
        return toString();
    }
}
