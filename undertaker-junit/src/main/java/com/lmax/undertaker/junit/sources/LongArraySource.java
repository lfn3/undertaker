package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Source;
import com.lmax.undertaker.junit.sources.LongSource;

import java.util.function.ToLongFunction;

public interface LongArraySource {
    default long[] getLongArray()
    {
        return getLongArray(LongSource::getLong);
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
