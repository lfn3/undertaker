package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.ToDoubleFunction;

public interface FloatArrayGen {
    default float[] getFloatArray()
    {
        return getFloatArray(FloatGen::getFloat);
    }
    default float[] getFloatArray(ToFloatFunction<Source> generator)
    {
        return getFloatArray(generator, 0, 64);
    }
    default float[] getFloatArray(ToFloatFunction<Source> generator, int minSize)
    {
        return getFloatArray(generator, minSize, minSize + 64);
    }
    float[] getFloatArray(ToFloatFunction<Source> generator, int minSize, int maxSize);
}
