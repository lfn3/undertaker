package com.lmax.undertaker.junit.generators;

import com.lmax.undertaker.junit.Source;

import java.util.function.Function;

import static com.lmax.undertaker.junit.generators.Generator.asGenerator;

@FunctionalInterface
public interface ObjectGen {
    <T> T generate(Generator<T> generator);

    default <T> T generate(Function<Source, T> f)
    {
        return this.generate(asGenerator(f));
    }
}
