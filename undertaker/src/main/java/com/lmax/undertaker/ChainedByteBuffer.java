package com.lmax.undertaker;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.stream.Collectors;

public class ChainedByteBuffer
{
    private int position = 0;
    private int mark = -1;
    private int limit = 0;
    private final List<ByteBuffer> buffers;

    private int stashedIndex;
    private ByteBuffer stashedBuffer;

    public ChainedByteBuffer()
    {
        buffers = new LinkedList<>();
    }

    public ChainedByteBuffer(ByteBuffer... buffers)
    {
        this.buffers = Arrays.asList(buffers);
        limit = Arrays.stream(buffers).mapToInt(Buffer::limit).reduce((a, b) -> a + b).orElse(0);
    }

    public ChainedByteBuffer add(ByteBuffer b)
    {
        buffers.add(b);
        return this;
    }

    public ChainedByteBuffer put(Byte b)
    {
        final ByteBuffer byteBuffer = buffers.get(position).put(b);
        possiblyMoveToNext(byteBuffer);

        return this;
    }

    /**
     * Move to the next byte buffer if we've reached the limit of that buffer and there's
     * another to move to. If there isn't we just stay at the last one, so we emit the
     * same error we would if overrunning a "normal" buffer on the next operation.
     * @param byteBuffer we're currently looking at
     */
    private void possiblyMoveToNext(ByteBuffer byteBuffer)
    {
        if (byteBuffer.position() == byteBuffer.limit() && position < buffers.size())
        {
            position++;
        }
    }

    public ChainedByteBuffer mark()
    {
        mark = position;
        buffers.get(position).mark();
        return this;
    }

    public ChainedByteBuffer reset()
    {
        position = mark;
        buffers.get(position).reset();
        return this;
    }

    public int limit()
    {
        return limit;
    }

    public byte[] array()
    {
        final byte[] output;
        final OptionalInt arrSize = buffers.stream().mapToInt(Buffer::limit).reduce((a, b) -> a + b);
        if (arrSize.isPresent())
        {
            output = new byte[arrSize.getAsInt()];
            int outputOffset = 0;
            for (ByteBuffer b : buffers)
            {
                final int size = b.limit();
                System.arraycopy(b.array(), 0, output, outputOffset, size);
                outputOffset += size;
            }
        }
        else
        {
            output = new byte[0];
        }
        return output;
    }

    public byte get()
    {
        return buffers.get(position).get();
    }

    public ByteBuffer last()
    {
        return buffers.get(buffers.size() - 1);
    }

    public void stash(ByteBuffer b)
    {
        if (stashedBuffer != null)
        {
            throw new IllegalStateException("Stash already contains " + printableByteBuffer(b));
        }
        this.stashedIndex = buffers.indexOf(b);
        if (stashedIndex == -1)
        {
            throw new IllegalArgumentException("Could not find buffer " + printableByteBuffer(b) + " by reference");
        }

        buffers.remove(stashedIndex);
        stashedBuffer = b;
    }

    public void unstash()
    {
        buffers.add(stashedIndex, stashedBuffer);
        discardStash();
    }

    public void discardStash()
    {
        stashedIndex = -1;
        stashedBuffer = null;
    }

    private static String printableByteBuffer(ByteBuffer buf)
    {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (byte b : buf.array()) {
            sb.append(b);
            sb.append(", ");
        }
        sb.append("]");
        return sb.toString();
    }

    @Override
    public String toString() {
        final String bufferString = buffers.stream()
                .map(ChainedByteBuffer::printableByteBuffer)
                .collect(Collectors.joining(", "));
        return "ChainedByteBuffer{" +
                "position=" + position +
                ", mark=" + mark +
                ", limit=" + limit +
                ", buffers=" + bufferString +
                '}';
    }
}
