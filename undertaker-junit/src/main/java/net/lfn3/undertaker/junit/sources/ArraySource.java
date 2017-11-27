package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Generator;

public interface ArraySource {
    default <T> T[] getArray(Class<T> klass, Generator<T> generator)
    {
        return getArray(klass, generator, 0, 64);
    }

    default <T> T[] getArray(Class<T> klass, Generator<T> generator, int max)
    {
        return getArray(klass, generator, 0, max);
    }

    <T> T[] getArray(Class<T> klass, Generator<T> generator, int min, int max);
}
