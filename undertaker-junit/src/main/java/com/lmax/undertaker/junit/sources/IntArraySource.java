package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToIntFunction;
import java.util.function.ToLongFunction;

public interface IntArraySource {
    default int[] getIntArray()
    {
        return getIntArray(IntSource::getInt);
    }
    default int[] getIntArray(ToIntFunction<Source> generator)
    {
        return getIntArray(generator, 0, 64);
    }
    default int[] getIntArray(ToIntFunction<Source> generator, int minSize)
    {
        return getIntArray(generator, minSize, minSize + 64);
    }
    int[] getIntArray(ToIntFunction<Source> generator, int minSize, int maxSize);
}
