package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Source;
import com.lmax.undertaker.junit.primitive.functions.ToShortFunction;

public interface ShortArraySource {
    default short[] getShortArray()
    {
        return getShortArray(ShortSource::getShort);
    }
    default short[] getShortArray(ToShortFunction<Source> generator)
    {
        return getShortArray(generator, 0, 64);
    }
    default short[] getShortArray(ToShortFunction<Source> generator, int minSize)
    {
        return getShortArray(generator, minSize, minSize + 64);
    }
    short[] getShortArray(ToShortFunction<Source> generator, int minSize, int maxSize);
}
