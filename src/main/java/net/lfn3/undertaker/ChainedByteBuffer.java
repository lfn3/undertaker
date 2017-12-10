package net.lfn3.undertaker;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.stream.Collectors;

public class ChainedByteBuffer implements Collection<ByteBuffer>
{
    private int position = 0;
    private int mark = -1;
    private final List<ByteBuffer> buffers;

    public ChainedByteBuffer()
    {
        buffers = new LinkedList<>();
    }

    public ChainedByteBuffer(ByteBuffer... buffers)
    {
        this.buffers = Arrays.asList(buffers);
    }

    @Override
    public boolean add(ByteBuffer b)
    {
        buffers.add(b);
        return true;
    }

    @Override
    public boolean remove(Object o) {
        return buffers.remove(o);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return buffers.containsAll(c);
    }

    @Override
    public boolean addAll(Collection<? extends ByteBuffer> c) {
        return buffers.addAll(c);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        return buffers.removeAll(c);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        return buffers.retainAll(c);
    }

    @Override
    public void clear() {
        buffers.clear();
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
        return buffers.stream().mapToInt(Buffer::limit).reduce((a, b) -> a + b).orElse(0);
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

    public List<ByteBuffer> subList(int from, int to)
    {
        return buffers.subList(from, to);
    }

    public void addAll(int at, ByteBuffer... buffers)
    {
        this.buffers.addAll(at, Arrays.asList(buffers));
    }

    @Override
    public int size()
    {
        return buffers.size();
    }

    @Override
    public boolean isEmpty() {
        return buffers.isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        return buffers.contains(o);
    }

    @Override
    public Iterator<ByteBuffer> iterator() {
        return buffers.iterator();
    }

    @Override
    public Object[] toArray() {
        return buffers.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return buffers.toArray(a);
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
                ", limit=" + limit() +
                ", buffers=" + bufferString +
                '}';
    }
}
