package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Generator;
import com.lmax.undertaker.junit.Source;

import java.util.function.Function;

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
