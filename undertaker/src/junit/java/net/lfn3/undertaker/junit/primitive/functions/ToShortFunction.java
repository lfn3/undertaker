package net.lfn3.undertaker.junit.primitive.functions;

@FunctionalInterface
public interface ToShortFunction<T> {
    short applyAsShort(T value);
}
