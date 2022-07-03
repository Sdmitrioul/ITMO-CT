package skroba.utils;

public interface Vec<T> {
	T get(int i);
	void set(int pos, T el);
	int size();
}
