package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Source;
import net.lfn3.undertaker.junit.primitive.functions.ToByteFunction;

public interface ByteArraySource {
    default byte[] getByteArray()
    {
        return getByteArray(ByteSource::getByte);
    }
    default byte[] getByteArray(ToByteFunction<Source> generator)
    {
        return getByteArray(generator, 0, 64);
    }
    default byte[] getByteArray(ToByteFunction<Source> generator, int minSize)
    {
        return getByteArray(generator, minSize, minSize + 64);
    }
    byte[] getByteArray(ToByteFunction<Source> generator, int minSize, int maxSize);
}
