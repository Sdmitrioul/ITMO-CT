package info.kgeorgiy.ja.skroba.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class HelloUDPServer implements HelloServer {
	protected static final String SOCKET_OPEN_EXCEPTION = "Can't open socket: ";
	protected static final String PACKET_CREATE_EXCEPTION = "Can't create datagram packet: ";
	protected static final String SOCKET_RECEIVING_EXCEPTION = "Exception while receiving request: ";
	protected static final String SOCKET_SENDING_EXCEPTION = "Exception while sending response: ";
	protected static final String PROGRAM_USAGE = "Format of launch: java HelloUDPServer <port> <threads>";
	
	protected static final int COUNT_OF_ARGUMENTS = 5;
	protected static final int SO_TIMEOUT_MILLISECONDS = 100;
	
	private DatagramSocket socket;
	private ExecutorService responders;
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(final int port, final int threads) {
		try {
			socket = new DatagramSocket(port);
		} catch (SocketException ex) {
			System.err.println(SOCKET_OPEN_EXCEPTION + ex.getMessage());
			return;
		}
		
		responders = Executors.newFixedThreadPool(threads);
		
		for (int i = 0; i < threads; i++) {
			responders.submit(createReceiveTask());
		}
	}
	
	private Runnable createReceiveTask() {
		return () -> {
			while (!socket.isClosed() && !Thread.currentThread().isInterrupted()) {
				try {
					final int bufferSize = socket.getReceiveBufferSize();
					final DatagramPacket request = UtilFunctions.getDatagramPacket(bufferSize);
					
					socket.receive(request);
					
					sendResponseTask(request);
				} catch (SocketException ex) {
					System.err.println(PACKET_CREATE_EXCEPTION + ex.getMessage());
				} catch (IOException ex) {
					System.err.println(SOCKET_RECEIVING_EXCEPTION + ex.getMessage());
				}
			}
		};
	}
	
	private void sendResponseTask(final DatagramPacket request) {
		final String requestMessage = UtilFunctions.getMessageFromPacket(request);
		final String responseMessage = "Hello, " + requestMessage;
		
		final DatagramPacket response = UtilFunctions.getDatagramPacket(
				responseMessage,
				request.getSocketAddress()
		);
		
		try {
			socket.send(response);
		} catch (IOException ex) {
			System.err.println(SOCKET_SENDING_EXCEPTION + ex.getMessage());
		}
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void close() {
		socket.close();
		UtilFunctions.shutdownAndAwaitTermination(responders);
	}
	
	public static void main(String[] args) {
		if (!UtilFunctions.validArguments(2, args)) {
			System.err.println("WRONG USAGE");
			System.err.println(PROGRAM_USAGE);
			return;
		}
		
		new HelloUDPServer().start(
				UtilFunctions.getArgumentParsedToInt(0, args),
				UtilFunctions.getArgumentParsedToInt(1, args)
		);
	}
}
