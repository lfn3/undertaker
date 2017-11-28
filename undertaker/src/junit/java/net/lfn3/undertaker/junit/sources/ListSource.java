package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Generator;

import java.util.List;

public interface ListSource
{
    default <T> List<T> getList(Generator<T> generator) {
        return getList(generator, 0, 64);
    }

    default <T> List<T> getList(Generator<T> generator, int max) {
        return getList(generator, 0, max);
    }

    <T> List<T> getList(Generator<T> generator, int min, int max);
}
