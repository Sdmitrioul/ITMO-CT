package info.kgeorgiy.ja.skroba.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.SocketAddress;
import java.net.SocketOption;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class UtilFunctions {
	private static final int STANDARD_WAITING_TIME = 60;
	private static final TimeUnit STANDARD_TIME_UNIT = TimeUnit.SECONDS;
	
	static int getArgumentParsedToInt(final int i, final String[] args) {
		try {
			return Integer.parseInt(args[i]);
		} catch (NumberFormatException ex) {
			throw new NumberFormatException("Wrong argument (must be int): " + (i + 1) + " - " + ex.getMessage());
		}
	}
	
	static boolean validArguments(final int countOfArguments, final String[] args) {
		return args != null
				&& args.length == countOfArguments
				&& Arrays.stream(args).allMatch(x -> x != null && !x.isEmpty());
	}
	
	static void shutdownAndAwaitTermination(final ExecutorService pool, final int waitingTime, final TimeUnit timeUnit) {
		//pool.shutdownNow();
		pool.shutdown();
		 try{
			if (!pool.awaitTermination(waitingTime, timeUnit)) {
				pool.shutdownNow();
				if (!pool.awaitTermination(waitingTime, timeUnit)) {
					System.err.println("Pool did not terminate");
				}
			}
		} catch (InterruptedException ie) {
			pool.shutdownNow();
			Thread.currentThread().interrupt();
		}
	}
	
	static void shutdownAndAwaitTermination(final ExecutorService pool) {
		shutdownAndAwaitTermination(pool, STANDARD_WAITING_TIME, STANDARD_TIME_UNIT);
	}
	
	static void shutdownAndAwaitTermination(final ExecutorService pool, final int waitingTime) {
		shutdownAndAwaitTermination(pool, waitingTime, STANDARD_TIME_UNIT);
	}
	
	static String getMessageFromPacket(final DatagramPacket packet) {
		return new String(
				packet.getData(),
				packet.getOffset(),
				packet.getLength(),
				StandardCharsets.UTF_8
		);
	}
	
	static DatagramPacket getDatagramPacket(final String message, final SocketAddress address) {
		return new DatagramPacket(
				message.getBytes(StandardCharsets.UTF_8),
				message.length(),
				address
		);
	}
	
	static DatagramPacket getDatagramPacket(final int size, final SocketAddress address) {
		return new DatagramPacket(
				new byte[size],
				size,
				address
		);
	}
	
	static DatagramPacket getDatagramPacket(final int size) {
		return new DatagramPacket(
				new byte[size],
				size
		);
	}
	
	static Selector getSelector() {
		try {
			Selector selector = Selector.open();
			return selector;
		} catch (IOException ex) {
			throw new RuntimeException("Exception, while opening selector: " + ex.getMessage());
		}
	}
	
	static void setTimeout(final Selector selector , final int timeoutMilliseconds) {
		try {
			selector.select(timeoutMilliseconds);
		} catch (IOException ex) {
			throw new RuntimeException("Exception, because of selector trouble: " + ex.getMessage());
		}
	}
	
	static String getMessageFromBuffer(final ByteBuffer buffer) {
		return StandardCharsets.UTF_8.decode(buffer).toString();
	}
	
	static void closeChannel(final DatagramChannel channel) {
		try {
			channel.close();
		} catch (IOException ex) {
			throw new RuntimeException("Can't close datagram channel: " + ex.getMessage());
		}
	}
	
	static void closeSelector(final Selector selector) {
		try	{
			selector.close();
		} catch (IOException ex) {
			throw new RuntimeException("Exception, while closing selector: " + ex.getMessage());
		}
	}
}
