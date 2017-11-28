package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Source;

import java.util.function.ToIntFunction;

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
