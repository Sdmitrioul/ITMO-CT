package info.kgeorgiy.ja.skroba.concurrent;

import java.util.*;

public class ThreadsResult<R> {
	private final List<R> results;
	private final Set<Integer> settled = new HashSet<>();
	private Integer counter;
	
	/**
	 * Constructor for resulting class, to use in parallel mapping. Class is synchronized.
	 * @param counter size of results
	 */
	public ThreadsResult(final int counter) {
		this.results = new ArrayList<>(Collections.nCopies(counter, null));
		this.counter = counter;
	}
	
	/**
	 * Set result on position.
	 * @param index setting position
	 * @param result setting object
	 */
	public synchronized void setResult(final int index, final R result) {
		checkIndex(index);
		results.set(index, result);
		notify();
	}
	
	private synchronized void checkIndex(final int index) {
		if (index < 0 || index >= results.size()) {
			throw new IllegalArgumentException("Index out of bound: index - " + index + "; size - " + results.size());
		}
		if (settled.contains(index)) {
			throw new IllegalArgumentException("Index position was already settled");
		}
		settled.add(index);
		counter--;
	}
	
	/**
	 * Results getter. Synchronized, return answer only when all results setts.
	 * @return list of results.
	 * @throws InterruptedException if method wait() throws it.
	 */
	public synchronized List<R> getResults() throws InterruptedException {
		while (counter > 0) {
			wait();
		}
		return results;
	}
}
