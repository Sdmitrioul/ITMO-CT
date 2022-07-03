package info.kgeorgiy.ja.skroba.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.StandardSocketOptions;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Iterator;
import java.util.Set;

import static info.kgeorgiy.ja.skroba.hello.ChannelAttachment.getStandardChannelAttachment;
import static info.kgeorgiy.ja.skroba.hello.UtilFunctions.setTimeout;

/**
 * Realization of {@link HelloClient} using {@link Selector} and {@link DatagramChannel}.
 */
public class HelloUDPNonblockingClient implements HelloClient {
	private static final String SOCKET_OPEN_EXCEPTION = "Can't open socket: ";
	private static final String SOCKET_INET_EXCEPTION = "Exception while sending request, or receiving response: ";
	private static final String CHANNEL_CLOSED_EXCEPTION = "Exception, because of channel closed: ";
	private static final String CHANNEL_REGISTER_EXCEPTION = "Exception while registering Datagram channel";
	private static final String PROGRAM_USAGE = "Format of launch: java ClassName <host> <port> <prefix> <threads> <requests>";
	
	private static final int COUNT_OF_ARGUMENTS = 5;
	private static final int SO_TIMEOUT_MILLISECONDS = 100;
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
		final SocketAddress socket = new InetSocketAddress(host, port);
		final Selector selector = UtilFunctions.getSelector();
		
		for (int i = 0; i < threads; i++) {
			try {
				final DatagramChannel channel = DatagramChannel.open().setOption(StandardSocketOptions.SO_REUSEADDR, true);
				channel.configureBlocking(false);
				channel.connect(socket);
				channel.register(selector, SelectionKey.OP_WRITE, getStandardChannelAttachment(i));
			} catch (ClosedChannelException ex) {
				System.err.println(CHANNEL_CLOSED_EXCEPTION + ex.getMessage());
				return;
			} catch (IOException ex) {
				System.err.println(CHANNEL_REGISTER_EXCEPTION + ex.getMessage());
				return;
			}
		}
		
		process(prefix, requests, socket, selector);
	}
	
	private void process(final String prefix, final int requests, final SocketAddress socket, final Selector selector) {
		while (!Thread.interrupted() && !selector.keys().isEmpty()) {
			setTimeout(selector, SO_TIMEOUT_MILLISECONDS);
			
			Set<SelectionKey> selectionKeys = selector.selectedKeys();
			
			if (selectionKeys.isEmpty()) {
				selector.keys()
						.forEach(key -> sendingRequest(socket, key, prefix));
				continue;
			}
			
			for (final Iterator<SelectionKey> iterator = selectionKeys.iterator(); iterator.hasNext(); ) {
				final SelectionKey key = iterator.next();
				if (key.isReadable()) {
					handlingResponse(socket, key, prefix, requests);
				} else if (key.isWritable()) {
					sendingRequest(socket, key, prefix);
				}
				iterator.remove();
			}
		}
	}
	
	private void sendingRequest(final SocketAddress socket, final SelectionKey key, final String prefix) {
		final DatagramChannel channel = (DatagramChannel) key.channel();
		final ChannelAttachment attachment = (ChannelAttachment) key.attachment();
		
		attachment.clearBuffer();
		
		try {
			channel.send(attachment.getByteBufferMessage(prefix), socket);
		} catch (IOException ex) {
			throw new RuntimeException(SOCKET_INET_EXCEPTION + ex.getMessage());
		}
		
		attachment.flipBuffer();
		key.interestOps(SelectionKey.OP_READ);
	}
	
	private void handlingResponse(final SocketAddress socket, final SelectionKey key, final String prefix, final int requests) {
		final DatagramChannel channel = (DatagramChannel) key.channel();
		final ChannelAttachment attachment = (ChannelAttachment) key.attachment();
		
		attachment.clearBuffer();
		
		try {
			channel.receive(attachment.getBuffer());
		} catch (IOException ex) {
			throw new RuntimeException(SOCKET_INET_EXCEPTION + ex.getMessage());
		}
		
		attachment.flipBuffer();
		
		final String request = attachment.getMessage(prefix);
		final String response = UtilFunctions.getMessageFromBuffer(attachment.getBuffer());
		
		if (response.contains(request)) {
			System.out.println("Request: " + request);
			System.out.println("Response: " + response);
			attachment.incrementRequestIndex();
		}
		
		key.interestOps(SelectionKey.OP_WRITE);
		
		if (attachment.getRequestIndex() == requests) {
			UtilFunctions.closeChannel(channel);
		}
	}
	
	public static void main(String[] args) {
		if (!UtilFunctions.validArguments(COUNT_OF_ARGUMENTS, args)) {
			System.err.println("WRONG USAGE");
			System.err.println(PROGRAM_USAGE);
			return;
		}
		
		new HelloUDPNonblockingClient()
				.run(
					args[0],
					UtilFunctions.getArgumentParsedToInt(1, args),
					args[2],
					UtilFunctions.getArgumentParsedToInt(3, args),
					UtilFunctions.getArgumentParsedToInt(4, args)
				);
	}
}
