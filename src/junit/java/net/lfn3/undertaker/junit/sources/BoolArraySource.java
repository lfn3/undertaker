package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Source;
import net.lfn3.undertaker.junit.primitive.functions.ToBoolFunction;

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
