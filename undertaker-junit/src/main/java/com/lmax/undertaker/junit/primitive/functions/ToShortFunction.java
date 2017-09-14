package com.lmax.undertaker.junit.primitive.functions;

@FunctionalInterface
public interface ToShortFunction<T> {
    short applyAsShort(T value);
}
