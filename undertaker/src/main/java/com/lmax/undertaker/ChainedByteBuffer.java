package com.lmax.undertaker;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ChainedByteBuffer
{
    private int position = 0;
    private int mark = -1;
    private final List<ByteBuffer> buffers;

    public ChainedByteBuffer()
    {
        buffers = new ArrayList<>();
    }

    public ChainedByteBuffer(ByteBuffer... buffers)
    {
        this.buffers = Arrays.asList(buffers);
    }

    public ChainedByteBuffer put(Byte b)
    {
        final ByteBuffer byteBuffer = buffers.get(position).put(b);
        if (byteBuffer.position() == byteBuffer.limit() && position < buffers.size())
        {
            position++;
        }

        return this;
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

    public byte get()
    {
        return buffers.get(position).get();
    }
}
