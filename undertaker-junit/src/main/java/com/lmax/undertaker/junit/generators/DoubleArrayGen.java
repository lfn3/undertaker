package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToDoubleFunction;

public interface DoubleArrayGen {
    default double[] getDoubleArray()
    {
        return getDoubleArray(DoubleGen::getDouble);
    }
    default double[] getDoubleArray(ToDoubleFunction<Source> generator)
    {
        return getDoubleArray(generator, 0, 64);
    }
    default double[] getDoubleArray(ToDoubleFunction<Source> generator, int minSize)
    {
        return getDoubleArray(generator, minSize, minSize + 64);
    }
    double[] getDoubleArray(ToDoubleFunction<Source> generator, int minSize, int maxSize);
}
