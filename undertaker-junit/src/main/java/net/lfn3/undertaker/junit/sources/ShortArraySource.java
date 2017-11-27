package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Source;
import net.lfn3.undertaker.junit.primitive.functions.ToShortFunction;

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
