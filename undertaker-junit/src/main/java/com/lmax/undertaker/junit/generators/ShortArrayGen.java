package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToIntFunction;

public interface ShortArrayGen {
    default short[] getShortArray()
    {
        return getShortArray(ShortGen::getShort);
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
