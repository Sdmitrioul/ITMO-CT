package info.kgeorgiy.ja.skroba.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Implementation of interface {@link ListIP}
 */
public class IterativeParallelism implements ListIP {
	private final ParallelMapper mapper;
	
	/**
	 * Empty constructor, this class uses it standard task mapper
	 */
	public IterativeParallelism() {
		this.mapper = null;
	}
	
	/**
	 * Constructor for class to use given mapper
	 * @param mapper task mapper
	 */
	public IterativeParallelism(final ParallelMapper mapper) {
		this.mapper = mapper;
	}
	
	private <T, U, R> R computeInThreads(final int threads, final List<T> values, final Function<Stream<T>, U> mapper,
										 final Function<Stream<U>, R> reducer) throws InterruptedException {
		final List<Stream<T>> streams = getStreams(threads, values);
		
		final List<U> results = this.mapper != null ?
				this.mapper.map(mapper, streams) : standardMapper(mapper, streams);
		
		return reducer.apply(results.stream());
	}
	
	private <T, U> List<U> standardMapper(final Function<Stream<T>, U> mapper, final List<Stream<T>> streams) throws InterruptedException {
		final int streamsCount = streams.size();
		
		final List<Thread> threadsList = new ArrayList<>(streamsCount);
		final List<U> results = new ArrayList<>(Collections.nCopies(streamsCount, null));
		
		for (int i = 0; i < streamsCount; i++) {
			addThread(i, streams.get(i), threadsList, results, mapper);
		}
		
		joinThreads(threadsList);
		return results;
	}
	
	private void joinThreads(final List<Thread> threadsList) throws InterruptedException {
		InterruptedException exception = null;
		for (Thread thread : threadsList) {
			try {
				thread.join();
			} catch (InterruptedException e) {
				if (exception == null) {
					exception = e;
				} else {
					exception.addSuppressed(e);
				}
			}
		}
		
		if (exception != null) {
			throw exception;
		}
	}
	
	private <T, U> void addThread(final int position, final Stream<T> stream,
							   final List<Thread> threadsList, final List<U> results, final Function<Stream<T>, U> mapper) {
		final Thread thread = new Thread(() -> results.set(position, mapper.apply(stream)));
		threadsList.add(thread);
		thread.start();
	}
	
	private <T> List<Stream<T>> getStreams(final int threads, final List<T> values) {
		final int streamsCount = Math.min(threads, values.size());
		final int streamSize = streamsCount != 0 ? values.size() / streamsCount : 0;
		
		List<Stream<T>> streams = new ArrayList<>(streamsCount);
		
		for (int i = 0; i < streamsCount; i++) {
			final int left = i * streamSize;
			final int right = i < streamsCount - 1 ? left + streamSize : values.size();
			
			streams.add(values.subList(left, right).stream());
		}
		
		return streams;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T> T maximum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
		return computeInThreads(
				threads,
				values,
				stream -> stream.max(comparator).get(),
				stream -> stream.max(comparator).get());
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T> T minimum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
		return computeInThreads(
				threads,
				values,
				stream -> stream.min(comparator).get(),
				stream -> stream.min(comparator).get());
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T> boolean all(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
		return computeInThreads(
				threads,
				values,
				stream -> stream.allMatch(predicate),
				stream -> stream.allMatch(Boolean::booleanValue));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T> boolean any(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
		return computeInThreads(
				threads,
				values,
				stream -> stream.anyMatch(predicate),
				stream -> stream.anyMatch(Boolean::booleanValue));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public String join(final int threads, final List<?> values) throws InterruptedException {
		return computeInThreads(
				threads,
				values,
				stream -> stream.map(x -> x.toString()).collect(Collectors.joining()),
				stream -> stream.collect(Collectors.joining()));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T> List<T> filter(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
		return computeInThreads(
				threads,
				values,
				stream -> stream.filter(predicate).collect(Collectors.toList()),
				stream -> stream.flatMap(List::stream).collect(Collectors.toList()));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public <T, U> List<U> map(final int threads, final List<? extends T> values, final Function<? super T, ? extends U> f) throws InterruptedException {
		return computeInThreads(
				threads,
				values,
				stream -> stream.map(f).collect(Collectors.toList()),
				stream -> stream.flatMap(List::stream).collect(Collectors.toList()));
	}
}
