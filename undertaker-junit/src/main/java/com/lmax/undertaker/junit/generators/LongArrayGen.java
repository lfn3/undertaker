package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToLongFunction;

public interface LongArrayGen {
    default long[] getLongArray()
    {
        return getLongArray(LongGen::getLong);
    }
    default long[] getLongArray(ToLongFunction<Source> generator)
    {
        return getLongArray(generator, 0, 64);
    }
    default long[] getLongArray(ToLongFunction<Source> generator, int minSize)
    {
        return getLongArray(generator, minSize, minSize + 64);
    }
    long[] getLongArray(ToLongFunction<Source> generator, int minSize, int maxSize);
}