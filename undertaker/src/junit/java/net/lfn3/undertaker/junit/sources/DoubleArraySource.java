package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Source;

import java.util.function.ToDoubleFunction;

public interface DoubleArraySource {
    default double[] getDoubleArray()
    {
        return getDoubleArray(DoubleSource::getDouble);
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
