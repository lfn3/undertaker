package com.lmax.undertaker.junit.sources;

import com.lmax.undertaker.junit.Generator;
import com.lmax.undertaker.junit.Source;

import java.util.function.Function;

import static com.lmax.undertaker.junit.Generator.asGenerator;

@FunctionalInterface
public interface ObjectSource {
    <T> T generate(Generator<T> generator);

    default <T> T generate(Function<Source, T> f)
    {
        return this.generate(asGenerator(f));
    }
}
