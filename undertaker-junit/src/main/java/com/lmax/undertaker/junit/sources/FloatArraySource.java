package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Source;
import com.lmax.undertaker.junit.primitive.functions.ToFloatFunction;

public interface FloatArraySource {
    default float[] getFloatArray()
    {
        return getFloatArray(FloatSource::getFloat);
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
