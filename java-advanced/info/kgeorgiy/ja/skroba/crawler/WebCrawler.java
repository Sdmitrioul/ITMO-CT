package info.kgeorgiy.ja.skroba.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;

@SuppressWarnings("SpellCheckingInspection")
public class WebCrawler implements Crawler {
	private static final String PROGRAM_USAGE = "Format of launch: java WebCrawler url [depth [downloads [extractors [perHost]]]]";
	private static final String DOWNLOADER_EXCEPTION = "Exciption while creating downloader or using it";
	private static final List<Integer> DEFAULT_VALUE_OF_NUM_PARAMS = List.of(1, 1, 1, 1);
	
	private final ConcurrentHashMap<String, HostDownloader> hostDownloaders;
	private final ExecutorService downloaderService;
	private final ExecutorService extractorService;
	private final Downloader downloader;
	private final int perHost;
	
	/**
	 * Constuctor for {@link WebCrawler}
	 *
	 * @param downloader used downloader.
	 * @param downloaders count of threads to download.
	 * @param extractors count of threads to extract links.
	 * @param perHost the maximum count of threads to send a request to the same host.
	 */
	public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
		this.downloader = downloader;
		this.downloaderService = Executors.newFixedThreadPool(downloaders);
		this.extractorService = Executors.newFixedThreadPool(extractors);
		this.perHost = perHost;
		this.hostDownloaders = new ConcurrentHashMap<>();
	}
	
	/**
	 * Creates a {@link WebCrawler} and runs on the arguments that are provided.
	 * Format of launch: java WebCrawler url [depth [downloads [extractors [perHost]]]].
	 * If there are less than one or more than five arguments, the arguments could not be parsed correctly,
	 * or an error occurs during crawling an error message is printed to the standard output.
	 *
	 * @param args provided arguments.
	 */
	public static void main(final String[] args) {
		if (!validArguments(args)) {
			System.err.println(PROGRAM_USAGE);
			return;
		}
		
		String url = args[0];
		int depth = getParsedArgument(1, args);
		int downloaders = getParsedArgument(2, args);
		int extractors = getParsedArgument(3, args);
		int perHost = getParsedArgument(4, args);
		
		try {
			Downloader downloader = new CachingDownloader();
			WebCrawler webCrawler = new WebCrawler(downloader, downloaders, extractors, perHost);
			webCrawler.download(url, depth);
		} catch (IOException e) {
			System.err.println(DOWNLOADER_EXCEPTION);
		}
	}
	
	private static int getParsedArgument(final int i, final String[] args) {
		if (args.length <= i) {
			return DEFAULT_VALUE_OF_NUM_PARAMS.get(i);
		}
		
		try {
			return Integer.parseInt(args[i]);
		} catch (NumberFormatException ex) {
			throw new NumberFormatException("wrong argumnet (must be int): " + i + " - " + ex.getMessage());
		}
	}
	
	private static boolean validArguments(final String[] args) {
		return args != null && args.length > 1 && args.length <= 5 && Arrays.stream(args).allMatch(x -> x != null && !x.isEmpty());
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public Result download(final String url, final int depth) {
		final Set<String> downloaded = ConcurrentHashMap.newKeySet();
		final Set<String> visits = ConcurrentHashMap.newKeySet();
		final Queue<String> inProcess = new ConcurrentLinkedDeque<>();
		final Map<String, IOException> errors = new ConcurrentHashMap<>();
		
		visits.add(url);
		inProcess.add(url);
		
		for (int i = 0; i < depth; i++) {
			final Phaser sync = new Phaser(1);
			
			recursiveWalk(i != depth, downloaded, errors, visits,  inProcess, sync);
			
			sync.arriveAndAwaitAdvance();
		}
		
		return new Result(List.copyOf(downloaded), errors);
	}
	
	// :NOTE: wait for termination
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void close() {
		shutdownAndAwaitTermination(downloaderService);
		shutdownAndAwaitTermination(extractorService);
	}
	
	private void shutdownAndAwaitTermination(final ExecutorService pool) {
		pool.shutdown();
		try {
			if (!pool.awaitTermination(5, TimeUnit.SECONDS)) {
				pool.shutdownNow();
				if (!pool.awaitTermination(5, TimeUnit.SECONDS)) {
					System.err.println("Pool did not terminate");
				}
			}
		} catch (InterruptedException ie) {
			pool.shutdownNow();
			Thread.currentThread().interrupt();
		}
	}
	
	
	
	private void recursiveWalk(final boolean extract, final Set<String> downloaded,
							   final Map<String, IOException> errors, final Set<String> visits,
							   final Queue<String> inProcess, final Phaser sync) {
		final int size = inProcess.size();
		for (int i = 0; i < size; i++) {
			final String url = inProcess.poll();
			
			try {
				final String host = URLUtils.getHost(url);
				HostDownloader hostDownloader = hostDownloaders
						.computeIfAbsent(host, s -> new HostDownloader(perHost, downloaderService));
				
				sync.register();
				hostDownloader.addTask(createDownloadTask(extract, url, downloaded, errors, visits, inProcess, sync, hostDownloader));
			} catch (final MalformedURLException ex) {
				errors.put(url, ex);
			}
		}
	}
	
	private Runnable createDownloadTask(final boolean extract, final String url, final Set<String> downloaded,
										final Map<String, IOException> errors, final Set<String> visits,
										final Queue<String> inProcess,
										final Phaser sync, final HostDownloader hostDownloader) {
		return () -> {
			try {
				Document document = downloader.download(url);
				downloaded.add(url);
				sync.register();
				if (extract) {
					extractorService.submit(createExtractionTask(downloaded, errors, visits, inProcess, sync, document));
				}
			} catch (final IOException ex) {
				errors.put(url, ex);
			} finally {
				sync.arrive();
				
				hostDownloader.next();
			}
		};
	}
	
	private Runnable createExtractionTask(final Set<String> downloaded,
										  final Map<String, IOException> errors, final Set<String> visits,
										  final Queue<String> inProcess,
										  final Phaser sync, final Document document) {
		return () -> {
			try {
				document.extractLinks().stream()
						.filter(link -> visits.add(link))
						.forEach(link -> inProcess.add(link));
			} catch (final IOException e) {
				//Ignore
			} finally {
				sync.arrive();
			}
		};
	}
}
