package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.Function;

public interface ArrayGen {
    default <T> T[] getArray(Class<T> klass, Function<Source, T> generator)
    {
        return getArray(klass, generator, 0, 64);
    }

    default <T> T[] getArray(Class<T> klass, Function<Source, T> generator, int max)
    {
        return getArray(klass, generator, 0, max);
    }

    <T> T[] getArray(Class<T> klass, Function<Source, T> generator, int min, int max);
}
