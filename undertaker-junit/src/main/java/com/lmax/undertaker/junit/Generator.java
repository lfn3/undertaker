package com.lmax.undertaker.junit;

import com.lmax.undertaker.junit.Source;

import java.util.function.Function;

@FunctionalInterface
public interface Generator<T> extends Function<Source, T> {
    static <U> Generator<U> asGenerator(Function<Source, U> f)
    {
        return f::apply;
    }
}
