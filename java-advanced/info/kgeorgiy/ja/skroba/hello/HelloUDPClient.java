package info.kgeorgiy.ja.skroba.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class HelloUDPClient implements HelloClient {
	private static final String SOCKET_OPEN_EXCEPTION = "Can't open socket: ";
	private static final String SOCKET_INET_EXCEPTION = "Exception while sending request, or receiving response: ";
	private static final String PROGRAM_USAGE = "Format of launch: java ClassName <host> <port> <prefix> <threads> <requests>";
	
	private static final int COUNT_OF_ARGUMENTS = 5;
	private static final int SO_TIMEOUT_MILLISECONDS = 200;
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
		final ExecutorService clientWorkers = Executors.newFixedThreadPool(threads);
		final InetSocketAddress socketAddress = new InetSocketAddress(host, port);
		
		for (int thread = 0; thread < threads; thread++) {
			clientWorkers.submit(createTask(thread, prefix, requests, socketAddress));
		}
		
		UtilFunctions.shutdownAndAwaitTermination(clientWorkers, threads * requests * 10);
	}
	
	private Runnable createTask(final int thread, final String prefix, final int requests, final InetSocketAddress socketAddress) {
		return () -> {
			try (DatagramSocket socket = new DatagramSocket()) {
				socket.setSoTimeout(SO_TIMEOUT_MILLISECONDS);
				
				for (int i = 0; i < requests; i++) {
					final String requestMessage = prefix + thread + "_" + i;
					
					final DatagramPacket requestPacket = UtilFunctions.getDatagramPacket(requestMessage, socketAddress);
					
					sendingRequest(socketAddress, socket, requestMessage, requestPacket);
				}
				
			} catch (SocketException ex) {
				System.err.println(SOCKET_OPEN_EXCEPTION + ex.getMessage());
			}
		};
	}
	
	private void sendingRequest(final InetSocketAddress socketAddress, final DatagramSocket socket, final String requestText, final DatagramPacket requestPacket) {
		while (!socket.isClosed() && !Thread.currentThread().isInterrupted()) {
			try	{
				socket.send(requestPacket);
				System.out.println("Request - " + requestText);
				
				final int bufferSize = socket.getReceiveBufferSize();
				final DatagramPacket responsePacket = UtilFunctions.getDatagramPacket(bufferSize, socketAddress);
				
				socket.receive(responsePacket);
				String responseMessage = UtilFunctions.getMessageFromPacket(responsePacket);
				
				if (responseMessage.contains(requestText)) {
					System.out.println("Response - " + responseMessage);
					return;
				}
			} catch (IOException e) {
				System.err.println(SOCKET_INET_EXCEPTION + e.getMessage());
			}
		}
	}
	
	public static void main(String[] args) {
		if (!UtilFunctions.validArguments(COUNT_OF_ARGUMENTS, args)) {
			System.err.println("WRONG USAGE");
			System.err.println(PROGRAM_USAGE);
			return;
		}
		
		new HelloUDPClient()
				.run(
						args[0],
						UtilFunctions.getArgumentParsedToInt(1, args),
						args[2],
						UtilFunctions.getArgumentParsedToInt(3, args),
						UtilFunctions.getArgumentParsedToInt(4, args)
				);
	}
}
