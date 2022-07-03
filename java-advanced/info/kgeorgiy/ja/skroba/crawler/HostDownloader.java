package info.kgeorgiy.ja.skroba.crawler;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.ExecutorService;

public class HostDownloader {
	private final ExecutorService downloaders;
	private final Queue<Runnable> tasks;
	private final int perHost;
	private int size;
	
	public HostDownloader(final int perHost, final ExecutorService downloaders) {
		this.tasks = new ArrayDeque<>();
		this.perHost = perHost;
		this.downloaders = downloaders;
	}
	
	public synchronized void next() {
		final Runnable task = tasks.poll();
		
		if (task == null) {
			size--;
			return;
		}
		
		downloaders.submit(task);
	}
	
	public synchronized void addTask(final Runnable task) {
		if (size < perHost) {
			downloaders.submit(task);
			size++;
			return;
		}
		
		tasks.add(task);
	}
}
