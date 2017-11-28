package net.lfn3.undertaker.junit;

import java.util.function.Function;

@FunctionalInterface
public interface Generator<T> extends Function<Source, T> {
    static <U> Generator<U> asGenerator(Function<Source, U> f)
    {
        return f::apply;
    }
}
