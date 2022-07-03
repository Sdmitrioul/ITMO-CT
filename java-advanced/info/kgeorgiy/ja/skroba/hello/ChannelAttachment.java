package info.kgeorgiy.ja.skroba.hello;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

/**
 *	Simple Attachment for datagram channel.
 */
public class ChannelAttachment {
	public final static int STANDARD_CAPACITY = 1024;
	
	private final int index;
	private int requestIndex;
	private final ByteBuffer buffer;
	
	public ChannelAttachment(final int index, final int requestIndex, final ByteBuffer buffer) {
		this.index = index;
		this.requestIndex = requestIndex;
		this.buffer = buffer;
	}
	
	public ChannelAttachment(final int index, final ByteBuffer buffer) {
		this(index, 0, buffer);
	}
	
	static ChannelAttachment getStandardChannelAttachment(final int index) {
		return new ChannelAttachment(index, ByteBuffer.allocate(STANDARD_CAPACITY));
	}
	
	public int getIndex() {
		return index;
	}
	
	public int getRequestIndex() {
		return requestIndex;
	}
	
	public ByteBuffer getBuffer() {
		return buffer;
	}
	
	public void clearBuffer() {
		buffer.clear();
	}
	
	public void flipBuffer() {
		buffer.flip();
	}
	
	public void incrementRequestIndex() {
		requestIndex++;
	}
	
	public String getMessage(final String prefix) {
		return prefix + index + "_" + requestIndex;
	}
	
	public ByteBuffer getByteBufferMessage(final String prefix) {
		return ByteBuffer.wrap(getMessage(prefix).getBytes(StandardCharsets.UTF_8));
	}
}
