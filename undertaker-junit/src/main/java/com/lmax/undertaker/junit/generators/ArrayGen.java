package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.Function;

public interface ArrayGen {
    <T> T[] getArray(Class<T> klass, Function<Source, T> generator);
}
