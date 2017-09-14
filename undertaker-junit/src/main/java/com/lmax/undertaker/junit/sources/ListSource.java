package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Generator;
import com.lmax.undertaker.junit.Source;

import java.util.List;
import java.util.function.Function;

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