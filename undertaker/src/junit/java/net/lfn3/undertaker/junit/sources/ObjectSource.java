package net.lfn3.undertaker.junit.sources;

import net.lfn3.undertaker.junit.Generator;
import net.lfn3.undertaker.junit.Source;

import java.util.function.Function;

import static net.lfn3.undertaker.junit.Generator.asGenerator;

public interface ObjectSource {
    <T> T generate(Generator<T> generator);

    <T> T generate(Class<T> aClass);

    default <T> T generate(Function<Source, T> f)
    {
        return this.generate(asGenerator(f));
    }
}
