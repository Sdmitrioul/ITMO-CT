package info.kgeorgiy.ja.skroba.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static info.kgeorgiy.ja.skroba.hello.UtilFunctions.setTimeout;

/**
 * Realization of {@link HelloServer} using {@link Selector} and {@link DatagramChannel}.
 */
public class HelloUDPNonblockingServer implements HelloServer {
	private static final String CHANNEL_OPEN_EXCEPTION = "Can't open channel: ";
	private static final String CHANNEL_BINDING_EXCEPTION = "Can't bind Datagram channel: ";
	private static final String SELECTOR_SELECT_EXCEPTION = "Exception, while waiting selected keys: ";
	private static final String CHANNEL_RECEIVE_EXCEPTION = "Exception, while receiving request: ";
	private static final String CHANNEL_SENDING_EXCEPTION = "Exception, while sending response: ";
	private static final String PROGRAM_USAGE = "Format of launch: java HelloUDPNonblockingServer <port> <threads>";
	
	private static final int COUNT_OF_ARGUMENTS = 5;
	private static final int SO_TIMEOUT_MILLISECONDS = 100;
	private static final int STANDARD_CAPACITY = 1024;
	
	private ExecutorService receiver;
	private Selector selector;
	private DatagramChannel channel;
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(final int port, final int threads) {
		try {
			channel = DatagramChannel.open();
			selector = Selector.open();
		} catch (IOException ex) {
			System.err.println(CHANNEL_OPEN_EXCEPTION + ex.getMessage());
			return;
		}
		
		final SocketAddress socket = new InetSocketAddress(port);
		
		receiver = Executors.newSingleThreadExecutor();
		
		try {
			channel.bind(socket);
			channel.configureBlocking(false);
			channel.register(selector, SelectionKey.OP_READ);
		} catch (IOException ex) {
			System.err.println(CHANNEL_BINDING_EXCEPTION + ex.getMessage());
			return;
		}
		
		receiver.submit(createReceiveTask());
	}
	
	private Runnable createReceiveTask() {
		return () -> {
			ByteBuffer buffer = ByteBuffer.allocate(STANDARD_CAPACITY);
			SocketAddress address = null;
			while (selector.isOpen() && !Thread.interrupted()) {
				setTimeout(selector, SO_TIMEOUT_MILLISECONDS);
				
				Set<SelectionKey> selectionKeys = selector.selectedKeys();
				
				for (final Iterator<SelectionKey> iterator = selectionKeys.iterator(); iterator.hasNext(); ) {
					final SelectionKey key = iterator.next();
					if (key.isReadable()) {
						address = getRequest(key, buffer);
					} else if (key.isWritable()) {
						sendResponse(key, buffer, address);
					}
					iterator.remove();
				}
			}
		};
	}
	
	private void sendResponse(final SelectionKey key, final ByteBuffer buffer, final SocketAddress address) {
		try {
			channel.send(buffer, address);
		} catch (IOException ex) {
			throw new RuntimeException(CHANNEL_SENDING_EXCEPTION + ex.getMessage());
		}
		key.interestOps(SelectionKey.OP_READ);
	}
	
	private SocketAddress getRequest(final SelectionKey key, final ByteBuffer buffer) {
		buffer.clear();
		SocketAddress socket = null;
		
		try {
			socket = channel.receive(buffer);
		} catch (IOException ex) {
			throw new RuntimeException(CHANNEL_RECEIVE_EXCEPTION + ex.getMessage());
		}
		
		buffer.flip();
		final String response = "Hello, " + UtilFunctions.getMessageFromBuffer(buffer);
		buffer.clear();
		buffer.put(response.getBytes(StandardCharsets.UTF_8));
		buffer.flip();
		key.interestOps(SelectionKey.OP_WRITE);
		
		return socket;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void close() {
		UtilFunctions.closeSelector(selector);
		UtilFunctions.closeChannel(channel);
		UtilFunctions.shutdownAndAwaitTermination(receiver);
	}
	
	public static void main(String[] args) {
		if (!UtilFunctions.validArguments(2, args)) {
			System.err.println("WRONG USAGE");
			System.err.println(PROGRAM_USAGE);
			return;
		}
		
		new HelloUDPNonblockingServer().start(
				UtilFunctions.getArgumentParsedToInt(0, args),
				UtilFunctions.getArgumentParsedToInt(1, args)
		);
	}
}
