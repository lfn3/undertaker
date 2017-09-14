package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Source;
import com.lmax.undertaker.junit.primitive.functions.ToByteFunction;

import java.util.function.ToLongFunction;

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
