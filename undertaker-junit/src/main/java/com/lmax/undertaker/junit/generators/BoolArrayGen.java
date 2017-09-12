package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToDoubleFunction;

public interface BoolArrayGen {
    default boolean[] getBoolArray()
    {
        return getBoolArray(BoolGen::getBool);
    }
    default boolean[] getBoolArray(ToBoolFunction<Source> generator)
    {
        return getBoolArray(generator, 0, 64);
    }
    default boolean[] getBoolArray(ToBoolFunction<Source> generator, int minSize)
    {
        return getBoolArray(generator, minSize, minSize + 64);
    }
    boolean[] getBoolArray(ToBoolFunction<Source> generator, int minSize, int maxSize);
}
