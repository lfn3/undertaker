package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToLongFunction;

public interface ByteArrayGen {
    default byte[] getByteArray()
    {
        return getByteArray(ByteGen::getByte);
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
