package info.kgeorgiy.ja.skroba.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

/**
 * Implementation of interface {@link ParallelMapper}
 */
public class ParallelMapperImpl implements ParallelMapper {
	private final Thread[] threads;
	private final Deque<Runnable> tasksQueue = new ArrayDeque<>();
	private final Runnable thread = () -> {
		try {
			while (!Thread.interrupted()) {
				final Runnable task;
				synchronized (tasksQueue) {
					while (tasksQueue.isEmpty()) {
						tasksQueue.wait();
					}
					task = tasksQueue.pollFirst();
					// :NOTE: notify() is okay here and less expensive
					tasksQueue.notify();
				}
				task.run();
			}
		} catch (InterruptedException ignored) {
			//Ignored
		}
	};
	
	/**
	 * Mapper constructor, create {@link threads} count of parallel threads, that waiting tasks.
	 * @param threads count of creating threads (must be positive).
	 */
	public ParallelMapperImpl(final int threads) {
		if (threads < 0) {
			throw new IllegalArgumentException("Threads count must be positive");
		}
		this.threads = new Thread[threads];
		
		for (int i = 0; i < threads; i++) {
			this.threads[i] = getNewThread();
			this.threads[i].start();
		}
	}
	
	private Thread getNewThread() {
		// :NOTE: do not create the runnable every time
		return new Thread(thread);
	}
	
	private void addTask(final Runnable task) {
		synchronized (tasksQueue) {
			tasksQueue.addLast(task);
			tasksQueue.notifyAll();
		}
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> values) throws InterruptedException {
		final int tasksSize = values.size();
		final ThreadsResult<R> results = new ThreadsResult<>(tasksSize);
		
		for (int i = 0; i < tasksSize; i++) {
			Runnable task = createTask(f, values.get(i), results, i);
			addTask(task);
		}
		
		return results.getResults();
	}
	
	private <T, R> Runnable createTask(final Function<? super T, ? extends R> f, final T value, final ThreadsResult<R> results, final int index) {
		return () -> results.setResult(index, f.apply(value));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void close() {
		for (Thread thread : threads) {
			thread.interrupt();
		}
		
		for (Thread thread : threads) {
			try {
				thread.join();
			} catch (InterruptedException ignored) {
				//ignored
			}
		}
	}
}
