package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Source;
import com.lmax.undertaker.junit.primitive.functions.ToBoolFunction;

import java.util.function.ToDoubleFunction;

public interface BoolArraySource {
    default boolean[] getBoolArray()
    {
        return getBoolArray(BoolSource::getBool);
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
